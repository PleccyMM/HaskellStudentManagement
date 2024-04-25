{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Main (main) where

import Lib
import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import qualified Data.ByteString.Lazy as B
import Control.Monad (mzero)

data Student = Student {
    firstName :: !Text,
    secondName :: !Text,
    age :: Int,
    modules :: [Text]
} deriving (Show, Generic)

instance FromJSON Student
instance ToJSON Student

jsonFile :: FilePath
jsonFile = "test.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO ()
main = do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Student])
    case d of
        Left err -> putStrLn err
        Right st -> print st