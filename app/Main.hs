{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main (main) where

import Lib
import GHC.Generics
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson (ToJSON, FromJSON, eitherDecode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B

data Student = Student {
    firstName :: !Text,
    secondName :: !Text,
    age :: Int,
    modules :: [Text]
} deriving (Show, Generic, ToJSON, FromJSON)

jsonFile :: FilePath
jsonFile = "test.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO ()
main = do
    d <- (eitherDecode <$> getJSON) :: IO (Either String [Student])
    case d of
        Left err -> Prelude.putStrLn err
        Right st -> print st
            


addStudent :: IO ()
addStudent = do
        d <- (eitherDecode <$> getJSON) :: IO (Either String [Student])
        case d of
            Left err -> Prelude.putStrLn err
            Right x -> do
                let st = Student { firstName = "Jessica", secondName = "Lovegood", age = 22, modules = ["BS0021", "MD3990"] }
                    updatedStudents = st : x
                I.writeFile jsonFile $ encodeToLazyText updatedStudents