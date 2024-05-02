{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Lib
    (Student(..)
    ,Module(..)
    ,jsonFileStudent
    ,getJSONStudent
    ,jsonFileModule
    ,getJSONModule
    ,fileCheck
    ,getAllStudents
    ,getAllModules
    ,findStudents
    ,checkTextStudent
    ,getFirstName
    ,getSecondName
    ,getStudentDetails
    ,getNumber
    ,isNumber'
    ,getModuleDetails
    ,getModules
    ,checkModules
    ,checkTextModule
    ,getCode
    ,convertAllStudents
    ,studentToString
    ) where

import Data.Aeson (ToJSON, FromJSON, eitherDecode, encode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Text
import qualified Data.Text.IO as I
import GHC.Generics
import System.Directory
import System.IO

data Student = Student {
    firstName :: !Text,
    secondName :: !Text,
    age :: Int,
    year :: Int,
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

findStudents :: String -> IO (Maybe Student)
findStudents t = do
    d <- getAllStudents
    return $ case d of
        Left err -> Nothing
        Right s -> checkTextStudent s (pack $ Prelude.map Data.Char.toLower t)

checkTextStudent :: [Student] -> Text -> Maybe Student
checkTextStudent [] _ = Nothing
checkTextStudent (x:xs) t = 
    if t == getFirstName x 
        then Just x 
        else if t == getSecondName x 
            then Just x 
            else checkTextStudent xs t 

getFirstName :: Student -> Text
getFirstName (Student { firstName = f }) = Data.Text.toLower f

getSecondName :: Student -> Text
getSecondName (Student { secondName = s }) = Data.Text.toLower s

getStudentDetails :: IO Student
getStudentDetails = do
    Prelude.putStrLn "First Name:"
    firstName <- I.getLine
    Prelude.putStrLn "Second Name:"
    secondName <- I.getLine
    age <- getNumber "Age:"
    year <- getNumber "Year of Study:"
    modules <- getModules
    return Student { firstName = firstName, secondName = secondName, age = age, year = year, modules = modules }

getNumber :: String -> IO Int
getNumber s = do
    Prelude.putStrLn s
    n <- Prelude.getLine
    if isNumber' n then return (read n) else do
        Prelude.putStrLn("Incorrect Format")
        getNumber s

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

convertAllStudents :: [Student] -> String
convertAllStudents [] = ""
convertAllStudents (x:xs) = studentToString x ++ convertAllStudents xs

studentToString :: Student -> String
studentToString (Student { firstName = f, secondName = s, age = a, year = y, modules = m }) = "First Name: " ++ unpack f ++ "\nSecond Name: " ++ unpack s ++ "\nAge: " ++ show a ++ "\nYear of Study: " ++ show y ++ "\nModules: " ++ Prelude.unwords (Prelude.map unpack m) ++ "\n\n"