{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main (main) where

import Lib
import System.Directory
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

data Module = Module {
    code :: !Text,
    name :: !Text
} deriving (Show, Generic, ToJSON, FromJSON)

jsonFileStudent :: FilePath
jsonFileStudent = "students.json"

getJSONStudent :: IO B.ByteString
getJSONStudent = do
    filePath <- fileCheck jsonFileStudent
    B.readFile filePath

jsonFileModule :: FilePath
jsonFileModule = "modules.json"

getJSONModule :: IO B.ByteString
getJSONModule = do
    filePath <- fileCheck jsonFileModule
    B.readFile filePath

fileCheck :: FilePath -> IO FilePath
fileCheck x = do
    fileExist <- doesFileExist x
    if not fileExist
    then do
            writeFile x "[]"
            return x
        else return x


addStudent :: IO ()
addStudent = do
        d <- (eitherDecode <$> getJSONStudent) :: IO (Either String [Student])
        case d of
            Left err -> Prelude.putStrLn err
            Right students -> do
                s <- getStudentDetails
                let updatedStudents = s : students
                B.writeFile jsonFileStudent (encode updatedStudents)

addModule :: IO ()
addModule = do
        d <- (eitherDecode <$> getJSONModule) :: IO (Either String [Module])
        case d of
            Left err -> Prelude.putStrLn err
            Right modules -> do
                m <- getModuleDetails
                let updatedModules = m : modules
                B.writeFile jsonFileModule (encode updatedModules)

getStudentDetails :: IO Student
getStudentDetails = do
    Prelude.putStrLn "First Name:"
    firstName <- I.getLine
    Prelude.putStrLn "Second Name:"
    secondName <- I.getLine
    age <- getAge
    Prelude.putStrLn "Modules (comma-separated):"
    modulesInput <- Prelude.getLine
    let modulesList = splitOn "," (pack modulesInput)
    return Student { firstName = firstName, secondName = secondName, age = age, modules = modulesList }

getAge :: IO Int
getAge = do
    Prelude.putStrLn "Age:"
    age <- Prelude.getLine
    if isNumber' age then return (read age) else getAge

isNumber' :: String -> Bool
isNumber' "" = True
isNumber' (x:xs) = if isDigit x then isNumber' xs else False


getModuleDetails :: IO Module
getModuleDetails = do
    Prelude.putStrLn "Module Code:"
    code <- I.getLine
    Prelude.putStrLn "Name:"
    name <- I.getLine
    return Module { code = code, name = name }

main :: IO ()
main = do
    d <- (eitherDecode <$> getJSONStudent) :: IO (Either String [Student])
    case d of
        Left err -> Prelude.putStrLn err
        Right st -> print st