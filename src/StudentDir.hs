{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module StudentDir (
    Student (..),
    Module (..),
    jsonFileStudent,
    getJSONStudent,
    jsonFileModule,
    getJSONModule,
    fileCheck,
    getAllStudents,
    getAllModules,
    findStudents,
    checkTextStudent,
    getFirstName,
    getSecondName,
    getAge,
    constructStudent,
    isInt,
    checkStudentOverlap,
    findSpecificStudent,
    clearModuleFromStudents,
    setStudentModules,
    remove,
    increaseYear,
    constructModule,
    checkModuleExistance,
    enrollModule,
    searchModules,
    checkModules,
    getCode,
    findCode,
    checkEnrolled,
    getStudentModules,
    countNumberOfLines,
    convertAllStudents,
    studentToString,
) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import qualified Data.ByteString.Lazy as B
import Data.Char
import Data.Text hiding (elem, filter, head, last, length, map, unwords, all, null)
import qualified Data.Text.IO as I
import GHC.Generics
import System.Directory

-- TYPES --

data Student = Student
    { firstName :: !Text
    , secondName :: !Text
    , age :: Int
    , year :: Int
    , modules :: [Text]
    }
    deriving (Show, Generic, ToJSON, FromJSON, Eq)

data Module = Module
    { code :: !Text
    , name :: !Text
    }
    deriving (Show, Generic, ToJSON, FromJSON, Eq)

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

findStudents :: String -> IO (Maybe [Student])
findStudents t = do
    d <- getAllStudents
    return $ case d of
        Left _ -> Nothing
        Right s -> Just (checkTextStudent s t)

checkTextStudent :: [Student] -> String -> [Student]
checkTextStudent [] _ = []
checkTextStudent (x : xs) t
    | t' == l (getFirstName x) = x : c
    | t' == l (getSecondName x) = x : c
    | otherwise = c
  where
    l = Data.Text.toLower
    t' = pack $ map Data.Char.toLower t
    c = checkTextStudent xs t

-- STUDENT DETAILS --

getFirstName :: Student -> Text
getFirstName (Student{firstName = f}) = f

getSecondName :: Student -> Text
getSecondName (Student{secondName = s}) = s

getAge :: Student -> Int
getAge (Student{age = a}) = a

getStudentModules :: Student -> [Text]
getStudentModules (Student{modules = t}) = t

-- ADDING STUDENTS --

constructStudent :: String -> String -> Int -> Int -> [String] -> IO Student
constructStudent fn ln a y m = do return $ Student (pack fn) (pack ln) a y (map pack m)

isInt :: String -> Bool
isInt s = not (null s) && all isDigit s

checkStudentOverlap :: Student -> [Student] -> Bool
checkStudentOverlap (Student{firstName = f, secondName = s, age = a}) st = case (findSpecificStudent st [f, s] a) of
    Nothing -> False
    Just _ -> True

-- DELETING FROM FILES --

findSpecificStudent :: [Student] -> [Text] -> Int -> Maybe Student
findSpecificStudent _ [] _ = Nothing
findSpecificStudent [] _ _ = Nothing
findSpecificStudent (x : xs) t a =
    if l (getFirstName x) == l (head t) && l (getSecondName x) == l (last t) && getAge x == a
        then Just x
        else findSpecificStudent xs t a
  where
    l = Data.Text.toLower

clearModuleFromStudents :: Text -> [Student] -> [Student]
clearModuleFromStudents _ [] = []
clearModuleFromStudents m (x : xs) = setStudentModules (remove m (getStudentModules x)) x : clearModuleFromStudents m xs

setStudentModules :: [Text] -> Student -> Student
setStudentModules m s = s{modules = m}

remove :: (Eq a) => a -> [a] -> [a]
remove _ [] = []
remove i (x : xs) = if i == x then c else x : c
  where
    c = remove i xs

-- INCREASE YEAR OF STUDY --

increaseYear :: Student -> Int -> Student
increaseYear s i = s{year = i}

-- ADDING MODULES --

constructModule :: String -> String -> IO Module
constructModule c n = do return $ Module (pack c) (pack n)

checkModuleExistance :: [String] -> IO Bool
checkModuleExistance m = do
    mods <- getAllModules
    case mods of
        Left err -> do
            putStrLn $ "Error fetching modules: " ++ err
            return False
        Right allModules -> do
            if searchModules (map pack m) allModules
                then return True
                else do
                    return False

enrollModule :: Text -> Student -> Student
enrollModule m s = do
    let mods = getStudentModules s
    s{modules = m : mods}

-- MODULE DETAILS --

searchModules :: [Text] -> [Module] -> Bool
searchModules [] _ = True
searchModules (x : xs) m = if checkModules m x then searchModules xs m else False

checkModules :: [Module] -> Text -> Bool
checkModules [] _ = False
checkModules (x : xs) t = if l t == l (getCode x) then True else checkModules xs t
  where
    l = Data.Text.toLower

getCode :: Module -> Text
getCode (Module{code = c}) = c

findCode :: [Module] -> Text -> Maybe Module
findCode [] _ = Nothing
findCode (x : xs) t = if getCode x == t then Just x else findCode xs t

-- ENROLLMENT PRINTING --

checkEnrolled :: [Student] -> Module -> String
checkEnrolled [] _ = ""
checkEnrolled (x : xs) m = if elem (getCode m) (getStudentModules x) then unpack (getFirstName x) ++ " " ++ unpack (getSecondName x) ++ "\n" ++ c else c
  where
    c = checkEnrolled xs m

countNumberOfLines :: String -> Int
countNumberOfLines s = length $ filter (== '\n') s

-- READABILITY PRINTING --

convertAllStudents :: [Student] -> String
convertAllStudents [] = ""
convertAllStudents (x : xs) = "\n" ++ studentToString x ++ "\n" ++ convertAllStudents xs

studentToString :: Student -> String
studentToString (Student{firstName = f, secondName = s, age = a, year = y, modules = m}) =
    "First Name: "
        ++ unpack f
        ++ "\nSecond Name: "
        ++ unpack s
        ++ "\nAge: "
        ++ show a
        ++ "\nYear of Study: "
        ++ show y
        ++ "\nModules: "
        ++ unwords (map unpack m)
