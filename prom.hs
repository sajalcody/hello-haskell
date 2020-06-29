{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE UnicodeSyntax #-}
module Prom where

import qualified Data.Aeson as D
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as DT
import Control.Exception
import Network.HTTP.Req

getPrometheusMetrics ∷ String → String → String → String → IO (Either HttpException (JsonResponse D.Value))
getPrometheusMetrics url userName passwd query = do
     let auth       = basicAuthUnsafe (B.pack userName) (B.pack passwd) <> (DT.pack "query" =: query)
         host       = http $ DT.pack url -- TODO: Support for both https and http
         -- portOption = maybe mempty port promPort
         pathPieces = map DT.pack ["api", "v1", "query"]
         urlScheme  = foldl (/:) host pathPieces
     try . runReq defaultHttpConfig . req GET urlScheme NoReqBody jsonResponse $ auth

main :: IO ()
main = do
    a <- getPrometheusMetrics "https://prometheus-prod.juspay.net" "grafana-user" "u7ChqX2LVQFBCj2sfZrYLD96XajPGvff" "({merchant_id=\"dream11\"})"
    case a of
        Left l-> putStrLn $ show l
        Right r -> showJsonResponse r

showJsonResponse:: (JsonResponse D.Value) -> (IO ())
showJsonResponse a = do
    x <- a
    print a
-- see :: Show a => JsonResponse a -> String
-- see res = case res of
--         JsonResponse a -> show a
--         _ -> "sample"

-- process :: IO ()
-- process  = do
--     a <- (getPrometheusMetrics "https://prometheus-prod.juspay.net" "grafana-user" "u7ChqX2LVQFBCj2sfZrYLD96XajPGvff" "({merchant_id=\"dream11\"})" )
--     case a of
--         Left _ -> putStrLn "sdjh"
--         Right k -> print $ (D.fromJSON . responseBody) k
    -- let b = fmap (D.fromJSON . responseBody) a
    -- putStrLn "sdjh"
    -- case a of
    --     Left l -> putStrLn "sample"
    --     Right r -> 
    --         case r of
    --             D.Error s -> putStrLn s
    --             D.Success b -> print b