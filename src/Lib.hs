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
    ,getAge
    ,getStudentDetails
    ,getNumber
    ,isInt
    ,findSpecificStudent
    ,clearModuleFromStudents
    ,setStudentModules
    ,remove
    ,increaseYear
    ,getModuleDetails
    ,getModules
    ,searchModules
    ,checkModules
    ,getCode
    ,findCode
    ,checkEnrolled
    ,getStudentModules
    ,countNumberOfLines
    ,convertAllStudents
    ,studentToString
    ) where

import Data.Aeson (ToJSON, FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Text hiding (length, filter, map, unwords, elem, head, last)
import qualified Data.Text.IO as I
import GHC.Generics
import System.Directory

-- TYPES --

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

-- FILE LOADING --

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

-- STUDENT SEARCHING --

findStudents :: String -> IO (Maybe Student)
findStudents t = do
    d <- getAllStudents
    return $ case d of
        Left _ -> Nothing
        Right s -> checkTextStudent s t

checkTextStudent :: [Student] -> String -> Maybe Student
checkTextStudent [] _ = Nothing
checkTextStudent (x:xs) t = 
    if t' == Data.Text.toLower (getFirstName x) 
        then Just x 
        else if t' == Data.Text.toLower (getSecondName x) 
            then Just x 
            else checkTextStudent xs t 
        where t' = (pack $ map Data.Char.toLower t)

-- STUDENT DETAILS --

getFirstName :: Student -> Text
getFirstName (Student { firstName = f }) = f

getSecondName :: Student -> Text
getSecondName (Student { secondName = s }) = s

getAge :: Student -> Int
getAge (Student { age = a }) = a

getStudentModules :: Student -> [Text]
getStudentModules (Student { modules = t }) = t

-- ADDING STUDENTS --

getStudentDetails :: IO Student
getStudentDetails = do
    putStrLn "First Name:"
    f <- I.getLine
    putStrLn "Second Name:"
    s <- I.getLine
    a <- getNumber "Age:"
    y <- getNumber "Year of Study:"
    m <- getModules
    return Student { firstName = f, secondName = s, age = a, year = y, modules = m }

getNumber :: String -> IO Int
getNumber s = do
    putStrLn s
    n <- getLine
    if isInt n then return (read n) else do
        putStrLn("Incorrect Format")
        getNumber s

isInt :: String -> Bool
isInt "" = True
isInt (x:xs) = if isDigit x then isInt xs else False

-- DELETING FROM FILES --

findSpecificStudent :: [Student] -> [Text] -> Int -> Maybe Student
findSpecificStudent [] _ _ = Nothing
findSpecificStudent (x:xs) t a = if l (getFirstName x) == l (head t) && l (getSecondName x) == l (last t) && getAge x == a 
                                    then Just x 
                                    else findSpecificStudent xs t a
                                        where l = Data.Text.toLower

clearModuleFromStudents :: Text -> [Student] -> [Student]
clearModuleFromStudents _ [] = []
clearModuleFromStudents m (x:xs) = setStudentModules (remove m (getStudentModules x)) x : clearModuleFromStudents m xs

setStudentModules :: [Text] -> Student -> Student
setStudentModules m s = s { modules = m} 

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove i (x:xs) = if i == x then c else x : c
    where c = remove i xs

-- INCREASE YEAR OF STUDY --

increaseYear :: Student -> Int -> Student
increaseYear s i = s { year = i }

-- ADDING MODULES --

getModuleDetails :: IO Module
getModuleDetails = do
    putStrLn "Module Code:"
    c <- I.getLine
    putStrLn "Name:"
    n <- I.getLine
    return Module { code = c, name = n }

getModules :: IO [Text]
getModules = do
    putStrLn "Modules (seperate with commas):"
    modulesInput <- getLine
    let m = splitOn "," (pack modulesInput)
    allModulesEither <- getAllModules
    case allModulesEither of
        Left err -> do
            putStrLn $ "Error fetching modules: " ++ err
            getModules
        Right allModules -> do
            if searchModules m allModules then return m else do
                putStrLn("Didn't Find Module")
                getModules

-- MODULE DETAILS --

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

-- ENROLLMENT PRINTING --

checkEnrolled :: [Student] -> Module -> String
checkEnrolled [] _ = ""
checkEnrolled (x:xs) m = if elem (getCode m) (getStudentModules x) then unpack (getFirstName x) ++ " " ++ unpack (getSecondName x) ++ "\n" ++ c else c
    where c = checkEnrolled xs m

countNumberOfLines :: String -> Int
countNumberOfLines s = length $ filter (=='\n') s

-- READABILITY PRINTING --

convertAllStudents :: [Student] -> String
convertAllStudents [] = ""
convertAllStudents (x:xs) = studentToString x ++ "\n\n" ++ convertAllStudents xs

studentToString :: Student -> String
studentToString (Student { firstName = f, secondName = s, age = a, year = y, modules = m }) = "First Name: " ++ unpack f ++ "\nSecond Name: " ++ unpack s ++ "\nAge: " ++ show a ++ "\nYear of Study: " ++ show y ++ "\nModules: " ++ unwords (map unpack m)