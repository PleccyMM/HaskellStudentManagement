{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main (main) where

import Lib
import GHC.Generics
import qualified Data.Text.IO as I
import Data.Text
import Data.Char
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
    age <- getAge
    Prelude.putStrLn "Modules (comma-separated):"
    modulesInput <- Prelude.getLine
    let modulesList = splitOn "," (pack modulesInput)
    return Student { firstName = firstName
                   , secondName = secondName
                   , age = age
                   , modules = modulesList
                   }

getAge :: IO Int
getAge = do
    Prelude.putStrLn "Age:"
    age <- Prelude.getLine
    if isNumber' age then return (read age) else getAge

isNumber' :: String -> Bool
isNumber' "" = True
isNumber' (x:xs) = if isDigit x then isNumber' xs else False