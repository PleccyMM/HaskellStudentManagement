{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main (main) where

import Lib
import Data.Aeson (ToJSON, FromJSON, eitherDecode, encode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Text
import qualified Data.Text.IO as I
import GHC.Generics
import System.Directory

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

getAllStudents :: IO (Either String [Student])
getAllStudents = (eitherDecode <$> getJSONStudent) 

getAllModules :: IO (Either String [Module])
getAllModules = (eitherDecode <$> getJSONModule)

searchStudent :: Text -> IO ()
searchStudent t = do
    d <- getAllStudents
    case d of
        Left err -> Prelude.putStrLn err
        Right s -> do
            let result = checkTextStudent s t 
            case result of
                Nothing -> Prelude.putStrLn "Didn't find student"
                Just st -> print st

checkTextStudent :: [Student] -> Text -> Maybe Student
checkTextStudent [] _ = Nothing
checkTextStudent (x:xs) t = 
    if t == getFirstName x 
        then Just x 
        else if t == getSecondName x 
            then Just x 
            else checkTextStudent xs t 

getFirstName :: Student -> Text
getFirstName (Student { firstName = f }) = f

getSecondName :: Student -> Text
getSecondName (Student { secondName = s }) = s

addStudent :: IO ()
addStudent = do
        d <- getAllStudents
        case d of
            Left err -> Prelude.putStrLn err
            Right students -> do
                s <- getStudentDetails
                let updatedStudents = s : students
                B.writeFile jsonFileStudent (encode updatedStudents)

addModule :: IO ()
addModule = do
        d <- getAllModules
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
    modules <- getModules
    return Student { firstName = firstName, secondName = secondName, age = age, modules = modules }

getAge :: IO Int
getAge = do
    Prelude.putStrLn "Age:"
    age <- Prelude.getLine
    if isNumber' age then return (read age) else do
        Prelude.putStrLn("Incorrect Format")
        getAge

isNumber' :: String -> Bool
isNumber' "" = True
isNumber' (x:xs) = if isDigit x then isNumber' xs else False

getModules :: IO [Text]
getModules = do
    Prelude.putStrLn "Modules (seperate with commas):"
    modulesInput <- Prelude.getLine
    let modules = splitOn "," (pack modulesInput)
    allModulesEither <- getAllModules
    case allModulesEither of
        Left err -> do
            putStrLn $ "Error fetching modules: " ++ err
            getModules
        Right allModules -> do
            if checkModules modules allModules then return modules else do
                Prelude.putStrLn("Didn't Find Module")
                getModules

checkModules :: [Text] -> [Module] -> Bool
checkModules [] _ = True
checkModules (x:xs) m = if checkTextModule m x then checkModules xs m else False

checkTextModule :: [Module] -> Text -> Bool
checkTextModule [] _ = False
checkTextModule (x:xs) t = if t == getCode x then True else checkTextModule xs t 

getCode :: Module -> Text
getCode (Module { code = c }) = c

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