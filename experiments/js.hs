{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import GHC.Generics
import Data.Aeson 
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy.Internal as BSL
import qualified Data.ByteString.Lazy.Char8 as BSCL
import Data.Text

data Person = Person {
      name :: [Char]
    , age  :: Int
    } deriving (Generic, Show)

instance ToJSON Person where
    -- No need to provide a toJSON implementation.

    -- For efficiency, we write a simple toEncoding implementation, as
    -- the default version uses toJSON.
    toEncoding = genericToEncoding defaultOptions
    

instance FromJSON Person where
    parseJSON = withObject "Person" $ \v -> Person
        <$> v .: "name"
        <*> v .: "age"

main :: IO () 
main = do
    let str =  "{\"name\":\"Joe\",\"age\":12}"
    let a = BSCL.pack str
    print $ decode' a