{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main (main) where

import Lib
import GHC.Generics
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I
import Data.Aeson (ToJSON, FromJSON, eitherDecode, encode)
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
            Right students -> do
                student <- getDetails
                let updatedStudents = student : students
                B.writeFile jsonFile (encode updatedStudents)
                

getDetails :: IO Student
getDetails = do
    Prelude.putStrLn "First Name:"
    firstName <- I.getLine
    Prelude.putStrLn "Second Name:"
    secondName <- I.getLine
    Prelude.putStrLn "Age:"
    age <- Prelude.getLine
    return Student {firstName=firstName, secondName=secondName, age=(read age), modules=["Hey"]}