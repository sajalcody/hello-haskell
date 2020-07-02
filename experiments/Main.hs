module Test.Main where

import qualified Data.HashMap.Strict as HM
import Data.Time.Clock
import Data.Time.Format
import Data.Txn
import Prelude
import Data.List
import Data.HashMap.Strict

--  TODO : Clean this up
sampleTxns :: IO [ Txn ]
sampleTxns = do
  let t = fromJust $ (splitOn "." >>> head >>> unpack >>> parseTimeM False defaultTimeLocale "%s-%m-%Y %X") (pack "21-05-2019 13:39:18.345")
  pure $
    (\i -> Txn
      { metrics = Metrics True
      , txnId = "foo"
      , txnsAppVersion = Just "bar"
      , orderId = Just "order"
      , createdAt = t
      , dimensions = HM.fromList [("merchant_id", pack ("m" ++ show @Int i)), ("gateway", "g")]
      }
    ) <$>[1..10000]

main = do
  ge <- googleEnv
  txns <- sampleTxns
  traverse (writeToBigQueryTable ge "godel-big-q" "euler_app_logs" "txns") txns
