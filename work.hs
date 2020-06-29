{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
module Getm where

import Data.Typeable
import qualified Data.Aeson as D
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as DT
import Control.Exception
import Network.HTTP.Req
import qualified Data.HashMap.Strict as HM

data Res = Res
  {
    gateway      :: String,
    insttance     :: String,
    job          :: String,
    payment_type :: String,
    merchant_id  :: String
  } deriving (Show, Eq)

getPrometheusMetrics ∷ String → String → String → String → IO (Either HttpException (JsonResponse D.Value))
getPrometheusMetrics url userName passwd query = do
     let auth       = basicAuthUnsafe (B.pack userName) (B.pack passwd) <> (DT.pack "query" =: query)
         host       = http $ DT.pack url -- TODO: Support for both https and http
         pathPieces = map DT.pack ["api", "v1", "query"]
         urlScheme  = foldl (/:) host pathPieces
     try . runReq defaultHttpConfig . req GET urlScheme NoReqBody jsonResponse $ auth

main :: IO ()
main = do
    -- x <- getPrometheusMetrics "prometheus-prod.juspay.net" (Just 9090) "grafana-user" "u7ChqX2LVQFBCj2sfZrYLD96XajPGvff" "({merchant_id=\"dream11\"})"
    let query = "sum_over_time(total_count{merchant_id=\"dream11\", gateway!=\"\", card_type=\"\", card_issuer_bank_name=\"\", payment_method=\"\", payment_type=\"online\",euler_txn_app_version=\"\",job=\"iris_vision_15\",instance=\"172.16.105.50:8000\",app_version=\"\"}[5m])"
    x <- getPrometheusMetrics "prometheus-prod.juspay.net" "grafana-user" "u7ChqX2LVQFBCj2sfZrYLD96XajPGvff" query
    let b = fmap (responseBody) x
    case b of
        Left _ -> putStrLn "There was exception"
        Right r -> 
            case r of
                D.Object obj -> putStrLn $ show $ HM.lookup "status" obj
    --print $ typeOf b