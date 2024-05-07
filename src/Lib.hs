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
    ,isInt
    ,getModuleDetails
    ,getModules
    ,searchModules
    ,checkModules
    ,getCode
    ,findCode
    ,checkEnrolled
    ,getStudentModules
    ,countNumberOfStudents
    ,convertAllStudents
    ,studentToString
    ) where

import Data.Aeson (ToJSON, FromJSON, eitherDecode, encode)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Text hiding (length, filter)
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
} deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Module = Module {
    code :: !Text,
    name :: !Text
} deriving (Show, Generic, ToJSON, FromJSON, Eq)

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
        Right s -> checkTextStudent s t

checkTextStudent :: [Student] -> String -> Maybe Student
checkTextStudent [] _ = Nothing
checkTextStudent (x:xs) t = 
    if t' == Data.Text.toLower (getFirstName x) 
        then Just x 
        else if t' == Data.Text.toLower (getSecondName x) 
            then Just x 
            else checkTextStudent xs t 
        where t' = (pack $ Prelude.map Data.Char.toLower t)

getFirstName :: Student -> Text
getFirstName (Student { firstName = f }) = f

getSecondName :: Student -> Text
getSecondName (Student { secondName = s }) = s

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
    if isInt n then return (read n) else do
        Prelude.putStrLn("Incorrect Format")
        getNumber s

isInt :: String -> Bool
isInt "" = True
isInt (x:xs) = if isDigit x then isInt xs else False

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
            if searchModules modules allModules then return modules else do
                Prelude.putStrLn("Didn't Find Module")
                getModules

searchModules :: [Text] -> [Module] -> Bool
searchModules [] _ = True
searchModules (x:xs) m = if checkModules m x then searchModules xs m else False

checkModules :: [Module] -> Text -> Bool
checkModules [] _ = False
checkModules (x:xs) t = if t == getCode x then True else checkModules xs t 

getCode :: Module -> Text
getCode (Module { code = c }) = c

findCode :: [Module] -> Text -> Maybe Module
findCode [] _ = Nothing
findCode (x:xs) t = if getCode x == t then Just x else findCode xs t

checkEnrolled :: [Student] -> Module -> String
checkEnrolled [] _ = ""
checkEnrolled (x:xs) m = if Prelude.elem (getCode m) (getStudentModules x) then unpack (getFirstName x) ++ " " ++ unpack (getSecondName x) ++ "\n" ++ c else c
    where c = checkEnrolled xs m

getStudentModules :: Student -> [Text]
getStudentModules (Student { modules = t }) = t

countNumberOfStudents :: String -> Int
countNumberOfStudents s = length $ filter (=='\n') s

convertAllStudents :: [Student] -> String
convertAllStudents [] = ""
convertAllStudents (x:xs) = studentToString x ++ convertAllStudents xs

studentToString :: Student -> String
studentToString (Student { firstName = f, secondName = s, age = a, year = y, modules = m }) = "First Name: " ++ unpack f ++ "\nSecond Name: " ++ unpack s ++ "\nAge: " ++ show a ++ "\nYear of Study: " ++ show y ++ "\nModules: " ++ Prelude.unwords (Prelude.map unpack m) ++ "\n\n"