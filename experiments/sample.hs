{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.Text


data Person = Person {
      name :: [Char]
    , age  :: Int
    } deriving Show


instance AT.FromJSON Person where
    parseJSON = AT.withObject "Person" $ \v -> Person
        <$> v AT..: "name"
        <*> v AT..: "age"
        -- >>> decode "{\"name\":\"Joe\",\"age\":12}" :: Maybe Person
        -- <interactive>:303:50-55: error:
        --     Not in scope: type constructor or class ‘Person’
        --

instance AT.ToJSON Person where
    toJSON (Person name age) = 
        AT.object [ "name"        AT..= name
                , "age"        AT..= age
                ]


jsonFile :: FilePath
jsonFile = "sample.json"

getJSON :: FilePath -> IO B.ByteString
getJSON jsonFile = B.readFile jsonFile


main :: IO ()
main = do
    let str = "{\"a\" : 1}"
    --let a = BS.unpack str
    --let b = B.pack a
    f <- B.readFile "sample.json"
    print  $ Aeson.decode f