{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnicodeSyntax #-}
module Main where

import Config
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Extra
import Data.Aeson.Text
import Data.Bifunctor
import Data.Default
import Data.Either
import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Time.Clock
import Network.HTTP.Req
import Network.HTTP.Types.Status
import Network.Wai.Middleware.RequestLogger
import Numeric.Extra
import Options.Applicative
import Prometheus
import System.IO                 (stdout)
import System.Log.Formatter      (simpleLogFormatter)
import System.Log.Handler        (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger         (Priority (DEBUG), debugM, rootLoggerName,
                                            setHandlers, setLevel, updateGlobalLogger)
import System.Process
import System.Exit
import Types.Config (GetMetricListResp(..), MetricResp(..), Metadata(..), DObject(..), Rule(..), ScheduledConfig(..))
import Utils
import Web.Scotty.TLS

import qualified Constants as Const
import qualified Data.Aeson as D
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.IORef as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.String as S
import qualified Data.Text as DT
import qualified Data.Text.Lazy.IO as LI
import qualified Data.Time.Format as TF
import qualified Data.Tuple.Extra as TE
import qualified Types.Config as T
import qualified Types.Custom as C
import qualified Types.PromMetrics as P
import qualified System.Directory as SD
import qualified System.Directory.Internal as SDI
import qualified System.Environment as E
import qualified System.Log.Logger as Log
import qualified Web.Scotty as WS

main ∷ IO ()
main = do
  -- Test HPA
  -- print $ encodeToLazyText $ createHpa "Deployment" "euler-kv-txn" "ec-prod" 1 10 [("custom_cpu", 50), ("custom_mem", 90)]
  --
  initLogging
  -- configs for prometheus and this code
  userName        ← E.getEnv "PROMETHEUS_USERNAME"
  passwd          ← E.getEnv "PROMETHEUS_PASSWORD"
  url             ← E.getEnv "PROMETHEUS_HOST"
  promPort        ← fmap read <$> E.lookupEnv "PROMETHEUS_PORT"
  sslcert         ← E.getEnv "SSL_CERT"
  sslkey          ← E.getEnv "SSL_KEY"
  depConfFile     ← E.getEnv "DEPLOY_MIN_CONF"
  deployRepCon    ← ifM (SD.doesFileExist depConfFile)
                        ((D.decode . S.fromString <$> readFile depConfFile) >>= maybe (fail "Decode failed for deployment min config") return)
                        (return def)
  s3url           ← E.getEnv "PLAN_CONFIG_FILE_LOC"
  stateFilePath   ← fromMaybe "/usr/share/autoscaler" <$> E.lookupEnv "STATE_FILE_PATH"
  sharedFilePath  ← fromMaybe "/usr/share/autoscaler/executedtasklist" <$> E.lookupEnv "SHARED_FILE_LOC"
  port            ← read . fromMaybe "3001" <$> E.lookupEnv "PORT"
  lastTime        ← parseUTCTime <$> ifM (SD.doesFileExist (stateFilePath <> "/backtracktime"))
                                        (readFile (stateFilePath <> "/backtracktime"))
                                        (return def)
  delayTime       ← case lastTime of
                        Just lt -> do
                          currentTime ← getCurrentTime
                          return $ if utctDay currentTime == utctDay lt
                                   then fromEnum (utctDayTime currentTime - utctDayTime lt)
                                   else fromEnum (utctDayTime currentTime)
                        Nothing -> return 0

  -- checks on env variables
  ifM (SD.doesFileExist sslcert `SDI.andM` SD.doesFileExist sslkey)
      (return ())
      (fail "cert or ssl key file is not available")

  if port == 9000
  then fail "You can't take 9000 port as it already assigned for prometheus"
  else putStrLn "Read environment variables successfully."

  -- scheduled and completed tasks storing variables
  list            ← ifM (SD.doesFileExist sharedFilePath)
                         (readFile sharedFilePath)
                         (return def)
  executedListRef ← I.newIORef (list, [])
  previousErrC    ← I.newIORef 0.0

  -- execution grace periods
  let executionGracePeriod = 3 -- 3 seconds

  -- initialize prometheus metrics
  totalC          ← register . counter $ Const.totalTaskCounterInfo
  errorC          ← register . counter $ Const.errorCounterInfo
  promErrorC      ← register . counter $ Const.prometheusErrorCounterInfo
  threadSkipC     ← register . counter $ Const.threadSkipCounterInfo
  threadDieC      ← register . counter $ Const.threadDiedCounterInfo
  scheduledG      ← register . gauge   $ Const.scheduledTaskCounterInfo
  defaultHpaC     ← register . counter $ Const.fallbackToDefaultHpaInfo
  scalerStats     ← register . vector ( DT.pack "current_min"
                                      , DT.pack "current_max"
                                      , DT.pack "affected_resource_name"
                                      , DT.pack "execution_time"
                                      , DT.pack "affected_namespace"
                                      , DT.pack "affected_resource_type"
                                      ) $ gauge Const.scalerStatsInfo

  let counters     = P.ExposedMetrics errorC threadDieC threadSkipC scheduledG totalC

  -- middle ware for logging all the requests
  logger ← mkRequestLogger def { outputFormat = Apache FromHeader }

  -- server for prometheus metrics scraping runs at 9000 port
  forkIO . startPromServer $ logger

  -- start server for custom_metrics
  forkIO . startServer port sslkey sslcert executedListRef logger url promPort userName passwd stateFilePath defaultHpaC deployRepCon $ promErrorC

  -- loop for schedules
  forever $ do
    count <- getCounter errorC
    prev  ← I.readIORef previousErrC
    when (count >= 5.0 && prev /= count && prev + 5 <= count) (sendEmailUsingAws count errorC *> I.writeIORef previousErrC count)
    configE ← try . getConfigFromUrl s3url $ errorC
    case configE of
      Right config →
        traverse_ (
                    handleSchedulesWithError s3url sharedFilePath stateFilePath executedListRef deployRepCon executionGracePeriod delayTime counters scalerStats
                  ) config
      Left  exp    → do
        Log.errorM "Error occured while reading config from URL: "  $ show (exp ∷ SomeException)
        incCounter errorC
    threadDelay (10 * 1000 * 1000) -- 10 seconds

initLogging :: IO ()
initLogging = do
    stdOutHandler <- streamHandler stdout DEBUG >>= \lh -> return $
            setFormatter lh (simpleLogFormatter "[$tid : $time : $prio ] $loggername : $msg")
    updateGlobalLogger rootLoggerName (setLevel DEBUG . setHandlers [stdOutHandler])

startPromServer logger = WS.scotty 9000 $ do
    WS.middleware                      logger
    WS.get (S.fromString "/")          $ exportMetricsAsText >>= WS.raw

startServer port sslkey sslcert ref logger url promPort userName passwd stateFilePath defHpaC deployRepCon promErrorC =
  scottyTLS port sslkey sslcert $ do
  --WS.scotty port $ do
    WS.middleware                                                                                               logger
    WS.get  (S.fromString "/")                                                                                  (WS.status status200 *> WS.text (S.fromString ""))
    WS.get  (S.fromString "/health")                                                                            (WS.status status200 *> WS.text (S.fromString "Health Check Successful"))
    WS.post (S.fromString "/checkConfig")                                                                       verifyConfig
    WS.get  (S.fromString "/apis/custom.metrics.k8s.io/v1beta1/")                                               buildGetMetricResp
    WS.get  (S.fromString "/apis/custom.metrics.k8s.io/v1beta1/namespaces/:namespace/:kind/:kindValue/:metric") handleCustomMetrics
    WS.get  (S.fromString "/scheduled")                                                                         (WS.text . S.fromString . show . snd =<< (lift . I.readIORef $ ref))

  where
    routeParam = WS.param . S.fromString

    extractMetric me = if null . T.result . T._data $ me
                       then Nothing
                       else let val = T._value . head . T.result . T._data $ me
                            in if null val
                               then Nothing
                               else resultToMaybe . D.fromJSON . last $ val

    getMetric depName ns fn = fmap (D.fromJSON . responseBody) <$> getPrometheusMetrics url promPort userName passwd (fn ns depName)

    iso8601 t = take 23 (TF.formatTime TF.defaultTimeLocale "%FT%T%Q" t) ++ "Z"

    handleCustomMetrics = do
      ns   ← routeParam "namespace"
      ki   ← routeParam "kind"
      kv   ← routeParam "kindValue"
      me   ← routeParam "metric"
      resp ← lift . fmap (fmap $ fmap extractMetric) . getMetric kv ns $ getQuery me
      time ← iso8601 <$> lift getCurrentTime
      case resp of
       Right (D.Success (Just r)) → WS.json $ buildResp (read r) ns ki kv me time
       Right (D.Success Nothing)  → lift (incCounter promErrorC) *> lift (Log.errorM "No value found for metric : " me) *>  handleCustomMetrics' ki kv ns
       Right (D.Error e)          → lift (incCounter promErrorC) *> lift (Log.errorM "Decode failed while reading prom metrics for : " me) *> handleCustomMetrics' ki kv ns
       Left exp                   → lift (incCounter promErrorC) *>
                                    lift (Log.errorM "Error occured while getting metrics from Prometheus: " $ show (exp ∷ SomeException)) *>
                                    handleCustomMetrics' ki kv ns

    handleCustomMetrics' ki kv ns = do
      let fileName = stateFilePath <> "/custom-hpa-" <> kv <> ".json"
      hpa ← lift $ D.fromJSON . fromJust . D.decode . S.fromString <$> readFile fileName
      case hpa of
        D.Error er → do
          lift $ incCounter promErrorC
          lift $ Log.errorM "Failure in getting current hpa for deployment: " kv
          WS.status status500
          WS.text (S.fromString "Internal Server Error")
        D.Success val → do
          deployments ← lift $ getDeployments ns (DT.pack kv) defHpaC
          traverse_ (applyDefaultHpa stateFilePath ki kv val ns) deployments

    applyDefaultHpa stateFilePath ki kv val ns depName = do
      let spec = C.spec val
          fileName = stateFilePath <> "/default-hpa-" <> kv <> ".json"
          min  = C.minReplicas spec
          max  = C.maxReplicas spec
          cpuP = maybe def (C.targetValue . C._object) . find (\cm -> "custom_cpu" == (C.metricName . C._object $ cm)) . C.metrics $ spec
          hpa  = createHpaDefault ki depName ns min max [("cpu", cpuP)]
          cmd  = "kubectl apply -f " <> fileName <> " -n " <> ns
      lift $ LI.writeFile fileName (encodeToLazyText hpa)
      (e, so, se) ←  lift $ readCreateProcessWithExitCode (shell cmd) ""
      if e == ExitSuccess
      then do
        lift $ incCounter defHpaC
        lift $ Log.infoM "Moved to default hpa config as prometheus is not available for deployment: " kv
        WS.status status200
        WS.text (S.fromString "")
      else do
        lift $ incCounter promErrorC
        lift $ Log.errorM "Failure in fallback to default hpa with only cpu metric for deployment: " (kv <> " with error: " <> se)
        WS.status status500
        WS.text (S.fromString "Internal Server Error")

    buildGetMetricResp = let resp = GetMetricListResp {
                                        T.g_kind = "APIResourceList",
                                        T.g_apiVersion = "v1",
                                        T.g_groupVersion = "custom.metrics.k8s.io/v1beta1",
                                        T.g_resources = []
                                    }
                          in WS.status status200 *> WS.json resp

    verifyConfig = do
      time       ← lift getCurrentTime
      configResp ← fmap D.fromJSON . D.decode . S.fromString . LB.unpack <$> WS.body
      case configResp of
        Just (D.Error e)   → WS.status status400 *> WS.text (S.fromString $ "Error In Config: " <> show e)
        Just (D.Success a) → do
          let v = join $ map (\sc ->
                                 case M.lookup (T.deployment sc) deployRepCon of
                                    Nothing -> [False]
                                    Just d  -> map (parseTimeOrCronAndCheck time (T.early_min d) (T.late_min d)) . T.schedules $ sc
                             ) . T.scalingActions $ a
              cpu = map (isJust . find (\cm -> "custom_cpu" == T.metric cm) . T.rules) . T.scalingActions $ a
          if and v && and cpu
          then WS.status status200 *> WS.text (S.fromString "Config Check Successful")
          else if and cpu
          then WS.status status500 *> WS.text (S.fromString "Config Check Failed, as replicas condition failed")
          else WS.status status500 *> WS.text (S.fromString "Config Check Failed, custom_cpu is required")
        _ -> WS.status status400 *> WS.text (S.fromString "Wrong Format")

handleSchedulesWithError s3url sharedFilePath persistFilePath ref deployRepCon executionGracePeriod delayTime exposedMetrics ssV sc = do
  eitherVal ← try $ handleSchedules s3url sharedFilePath persistFilePath ref deployRepCon executionGracePeriod delayTime exposedMetrics ssV sc
  when (isLeft eitherVal) $ do
      Log.errorM "Error occured while handling schedules: " $ show (fromLeft' eitherVal ∷ SomeException)
      incCounter $ P.errorC exposedMetrics

handleSchedules s3url sharedFilePath persistFilePath ref deployRepCon executionGracePeriod delayTime exposedMetrics ssV sc = do
  let custom_metrics = map extract (T.rules sc)
  deployments ← getDeployments (T.namespace sc) (DT.pack . T.deployment $ sc) (P.errorC exposedMetrics)
  traverse_ (\depName → traverse_ (schedule custom_metrics depName) (T.schedules sc)) deployments

    where
      schedule cm depName rc = do
        currentTime ← getCurrentTime
        case parseUTCTime . T.time $ rc of
          Just scheduledTime -> runSchedule deployRepCon currentTime scheduledTime cm depName rc (T.time rc)
          Nothing -> do
            allTimes ← getAllSchedueledTimes (DT.pack $ T.time rc) delayTime currentTime
            if null allTimes
            then Log.infoM "No matching schedules for: " $ T.time rc
            else traverse_ (\time → runSchedule deployRepCon currentTime time cm depName rc (T.time rc)) allTimes

      runSchedule deployRepCon currentTime scheduledTime cm depName rc time'= do
          let time      = show scheduledTime
              taskName  = depName <> "-" <> T.namespace sc <> "-" <> time <> Const.taskDelimeter
          currentTime    ← getCurrentTime
          alreadyDone    ← isTaskAlreadyExecutedOrScheduled executionGracePeriod currentTime taskName
          unless alreadyDone $
            if currentTime >= scheduledTime
               then let minC      = case M.lookup depName deployRepCon of
                                         Just v -> if parseTimeOrCronAndCheck currentTime (T.early_min v) (T.late_min v) rc
                                                   then T.min rc
                                                   else T.early_min v
                                         Nothing -> T.min rc
                        hpaconfig = createHpa "Deployment" depName (T.namespace sc) minC (T.max rc) cm
                      in applyHpa (Just taskName) hpaconfig depName
            else do
              tid ← forkFinally (do
                      let diff  = diffUTCTime scheduledTime currentTime
                          delay = round (realToFrac (diff * 1000000) ∷ Double) ∷ Int
                      incGauge (P.scheduledG exposedMetrics)
                      Log.infoM "Task Scheduled: " $ taskName <> ", at: " <> show scheduledTime
                      threadDelay delay

                      -- after the delay
                      config ← getConfigFromUrl s3url . P.errorC $ exposedMetrics
                      currentTime ← getCurrentTime
                      let currentConfig = find (\c → DT.pack (T.deployment c) `DT.isPrefixOf` DT.pack depName) config
                          currentSced   = currentConfig >>= find (\schedule → time' == T.time schedule) . T.schedules
                      when (isJust currentSced) (runWithUpdatedConfig currentSced currentConfig (Just taskName) depName deployRepCon currentTime)
                    ) (onScheduledThreadTermination taskName currentTime)
              I.atomicModifyIORef ref (\(l,s) → ((l, (taskName, scheduledTime, tid):s), ()))
              return ()

      onScheduledThreadTermination taskName cr (Right val) = decGauge (P.scheduledG exposedMetrics) *> Log.infoM "Executing scheduled task for: " taskName
      onScheduledThreadTermination taskName cr (Left e)    = do
        tid ← myThreadId
        I.atomicModifyIORef ref (\(l,s) → ((l, L.deleteBy (\(t,c,i) (tN,cN,iN) -> t == tN) (taskName, cr, tid) s), ())) -- Deleted from scheduled tasks, it will be scheduled again automatically
        decGauge (P.scheduledG exposedMetrics)
        incCounter (P.totalC exposedMetrics)
        incCounter (P.errorC exposedMetrics)
        Log.errorM "Error. Terminating scheduled task! StackTrace:" $ show e

      runWithUpdatedConfig cs currentConfig taskNameUpd depName deployRepCon currentTime = do
        let rc'          = fromJust cs
            sc'          = fromJust currentConfig
            cm'          = map extract (T.rules sc')
            minC = case M.lookup depName deployRepCon of
                              Just v -> if parseTimeOrCronAndCheck currentTime (T.early_min v) (T.late_min v) rc'
                                        then T.min rc'
                                        else T.early_min v
                              Nothing -> T.min rc'
            hpaconfigUpd = createHpa "Deployment" depName (T.namespace sc) minC (T.max rc') cm'
        applyHpa taskNameUpd hpaconfigUpd depName

      applyHpa ∷ forall a. D.ToJSON a => Eq a => D.FromJSON a => Default a => Maybe String → C.HPAConfig a → String → IO ()
      applyHpa mtaskName conf depName = do
        let fileName = persistFilePath <> "/custom-hpa-" <> depName <> ".json"
            retrieve = showString "kubectl get hpa -o json -n " . showString (T.namespace sc) . showString " " . showString depName $ "-hpa"
            cmd      = "kubectl apply -f " <> fileName <> " -n " <> T.namespace sc
        (e, so, se)  ← readCreateProcessWithExitCode (shell retrieve) ""
        existingHpaM ← if e == ExitSuccess
                                then return . fromMaybe (Just def) . D.decode . S.fromString $ so
                                else if "NotFound" `L.isInfixOf` se
                                then Log.infoM "No default HPA available for " depName $> Just def
                                else incCounter (P.errorC exposedMetrics) *> Log.errorM "Not able to get current HPA for: " se $> Nothing
        when (isJust existingHpaM) $ do
          let existingHpa = fromJust existingHpaM
              spec = C.spec conf
          if conf /= existingHpa
          then do
            LI.writeFile fileName (encodeToLazyText conf)
            (e, so, se) ← readCreateProcessWithExitCode (shell cmd) ""
            counterV ← if e == ExitSuccess
              then do
                Log.infoM "Applying HPA for deployment: " depName
                case mtaskName of
                  Just taskName → do
                    appendFile sharedFilePath taskName
                    currentTime ← getCurrentTime
                    tid ← myThreadId
                    writeFile (persistFilePath <> "/backtracktime") (show currentTime)
                    I.atomicModifyIORef ref (\(l,s) → ((mappend taskName l, L.deleteBy (\(t,c,i) (tN,cN,iN) -> t == tN) (taskName, currentTime, tid) s), ()))
                  Nothing → return ()
                incCounter (P.totalC exposedMetrics)
                return 1.0
              else
                Log.errorM "Error occured while trying to appply HPA For deployment: " (depName <> " Error: " <> se)
                         *> incCounter (P.errorC exposedMetrics)
                         $> 0.0
            time <- show <$> getCurrentTime
            withLabel ssV ( DT.pack . show . C.minReplicas $ spec
                          , DT.pack . show . C.maxReplicas $ spec
                          , DT.pack . C.h_name . C.scaleTargetRef $ spec
                          , DT.pack time
                          , DT.pack . C.namespace . C.metadata $ conf
                          , DT.pack "deployment"
                          ) (`setGauge` counterV)
          else Log.infoM "No change in the HPA config for deployment: " depName

      isTaskAlreadyExecutedOrScheduled executionGracePeriod currentTime taskName = do
        (x,y) ← I.readIORef ref
        if taskName `L.isInfixOf` x
        then Log.infoM "Task already executed: " (Const.taskDelimeter <> taskName) $> True
        else let tup = find (\(t,c,i) -> t == taskName) y
              in if isJust tup
                 then if diffUTCTime currentTime (TE.snd3 (fromJust tup)) < executionGracePeriod
                      then Log.infoM "Task already scheduled: " (Const.taskDelimeter <> taskName) $> True
                      else do
                        killThread (TE.thd3 (fromJust tup))
                        incCounter (P.threadSkipC exposedMetrics)
                        Log.errorM "Scheduled task didn't execute properly: " (Const.taskDelimeter <> taskName <> " " <> show (TE.snd3 (fromJust tup))) $> False
                 else Log.infoM "Hurray, new task found: " (Const.taskDelimeter <> taskName) $> False

      extract (Rule t d) = (t, ceiling d)


getPrometheusMetrics ∷ ∀ e. Exception e ⇒ String → Maybe Int → String → String → String → IO (Either e (JsonResponse D.Value))
getPrometheusMetrics url promPort userName passwd query = do
     let auth       = basicAuthUnsafe (B.pack userName) (B.pack passwd) <> (DT.pack "query" =: query)
         host       = http $ DT.pack url -- TODO: Support for both https and http
         portOption = maybe mempty port promPort
         pathPieces = map DT.pack ["api", "v1", "query"]
         urlScheme  = foldl (/:) host pathPieces
     try . runReq def . req GET urlScheme NoReqBody jsonResponse $ auth <> portOption

getCurrentReplicaCount ∷ String → String → Counter → IO (Maybe Int)
getCurrentReplicaCount namespace depName erC = do
  let cmd = "kubectl get deploy " <> depName <> " -o json -n " <> namespace
  Log.infoM "Executing shell command: " cmd
  (e, so, se) ← readCreateProcessWithExitCode (shell cmd) ""
  if e == ExitSuccess
  then
    case D.decode . S.fromString $ so of
      Nothing → Log.infoM "No available replicas found!" depName $> Nothing
      Just a → return . T.availableReplicas . T.deployStatus $ a
  else do
    incCounter erC
    Log.errorM "Something went wrong while getting replicas: " se $> Nothing

getConfigFromUrl ∷ String → Counter → IO [ScheduledConfig]
getConfigFromUrl s3url erC = do
    configResp  ← D.fromJSON . responseBody <$> getConfig s3url
--    configResp ← D.fromJSON . fromJust . D.decode . S.fromString <$> readFile "config/config.json"
    case configResp of
      D.Error e   → incCounter erC *> Log.errorM "Error occured while reading config: " (show e) $> []
      D.Success a → Log.infoM "Downloaded new config." "" $> T.scalingActions a

getDeployments ∷ String → Text → Counter → IO [String]
getDeployments namespace deployRegex erC = do
  let cmd = "kubectl get deploy -o json -n " <> namespace
  (e, so, se) ← readCreateProcessWithExitCode (shell cmd) ""
  if e == ExitSuccess
  then
    case D.decode . S.fromString $ so of
      Nothing → Log.infoM "No deployments found! for: " "deployRegex" $> []
      Just a → return . filter (\v → deployRegex `DT.isPrefixOf` DT.pack v) . map (T.name . T._metadata) . T._items $ a
  else
    incCounter erC *> Log.errorM "Not able to get deployments matching given name: " (DT.unpack deployRegex <> " Error: " <> se) $> []
