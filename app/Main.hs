{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main (main
            ,searchStudents
            ,addStudent
            ,deleteStudent
            ,addModule
            ,deleteModule
            ,changeYear
            ,printStudentInfo
            ,printModuleInfo
            ,help
            ) where

import StudentDir
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as B
import Data.Text hiding (map, words)
import System.IO
import System.Environment (getArgs)

searchStudents :: String -> IO ()
searchStudents s = do 
    d <- findStudents s
    case d of
        Nothing -> putStrLn "Didn't find any students"
        Just st -> putStrLn $ convertAllStudents st

addStudent :: String -> String -> Int -> Int -> String -> IO ()
addStudent fn ln a y m = do
    d <- getAllStudents
    case d of
        Left err -> putStrLn err
        Right st -> do
            modulesExist <- checkModuleExistance (words m)
            if modulesExist
                then do
                    s <- constructStudent fn ln a y (words m)
                    if checkStudentOverlap s st
                        then putStrLn "Duplicate student"
                        else do
                            let updatedStudents = s : st
                            B.writeFile jsonFileStudent (encodePretty updatedStudents)
                            putStrLn "Student added successfully."
                else putStrLn "Didn't find all the module codes"
                        

deleteStudent :: [String] -> Int -> IO ()
deleteStudent i a = do
    d <- getAllStudents
    case d of
        Left err -> putStrLn err
        Right st -> do
            let d' = findSpecificStudent st (map pack i) a
            case d' of
                Nothing -> putStrLn "Couldn't find student"
                Just s -> do
                    let updatedStudents = remove s st
                    B.writeFile jsonFileStudent (encodePretty updatedStudents)
    
addModule :: IO ()
addModule = do
        d <- getAllModules
        case d of
            Left err -> putStrLn err
            Right mods -> do
                m <- getModuleDetails
                if checkModuleOverlap m mods then putStrLn "Duplicate module" else do
                    let updatedModules = m : mods
                    B.writeFile jsonFileModule (encodePretty updatedModules)

deleteModule :: String -> IO ()
deleteModule i = do
    d <- getAllModules
    case d of
        Left err -> putStrLn err
        Right mods -> do
            let d' = findCode mods (pack i)
            case d' of
                Nothing -> putStrLn "Couldn't find module"
                Just m -> do
                    d'' <- getAllStudents
                    case d'' of
                        Left err -> putStrLn err
                        Right st -> do
                            let updatedStudents = clearModuleFromStudents (pack i) st
                            let updatedModules = remove m mods
                            B.writeFile jsonFileStudent (encodePretty updatedStudents)
                            B.writeFile jsonFileModule (encodePretty updatedModules)

changeYear :: Int -> [String] -> Int -> IO ()
changeYear y i a = do
    d <- getAllStudents
    case d of
        Left err -> putStrLn err
        Right st -> do
            let d' = findSpecificStudent st (map pack i) a
            case d' of
                Nothing -> putStrLn "Couldn't find student"
                Just s -> do
                    let updatedStudents = remove s st ++ [increaseYear s y]
                    B.writeFile jsonFileStudent (encodePretty updatedStudents)

printStudentInfo :: IO ()
printStudentInfo = do
    inputHandle <- openFile "AllStudentInfo.txt" WriteMode
    d <- getAllStudents
    case d of
        Left err -> putStrLn err
        Right st -> hPutStr inputHandle $ convertAllStudents st
    hClose inputHandle

printModuleInfo :: String -> IO ()
printModuleInfo s = do
    inputHandle <- openFile (s ++ "ModuleInfo.txt") WriteMode
    d <- getAllModules
    case d of
        Left err -> putStrLn err
        Right ms -> do
            let m = findCode ms $ pack s
            case m of
                Nothing -> putStrLn "Didn't find module code"
                Just mc -> do
                    d' <- getAllStudents
                    case d' of
                        Left err -> putStrLn err
                        Right st -> hPutStr inputHandle ("Module " ++ s ++ " Enrollment:\n\n" ++ c ++ "\nNumber of Students: " ++ show (countNumberOfLines c))
                            where c = checkEnrolled st mc
    hClose inputHandle

help :: IO ()
help = putStrLn ("\nsearchStudents <first or last name>" ++
                "\naddStudent" ++
                "\ndeleteStudent <first name> <last name> <year>" ++ 
                "\naddModule" ++
                "\ndeleteModule <code>" ++
                "\nchangeYear <year> <first name> <last name> <current year>" ++
                "\nprintStudentInfo" ++
                "\nprintModuleInfo <code>\n")

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("searchStudents":name:_) -> searchStudents name
        ("addStudent":firstName:lastName:age:year:modules:_) -> 
            if isInt age && isInt year
                then addStudent firstName lastName (read age) (read year) modules
                else putStrLn "Ensure that the age and year and entered correctly"
        ("deleteStudent":firstName:lastName:year:_) -> deleteStudent [firstName, lastName] (read year)
        ("addModule":_) -> addModule
        ("deleteModule":code:_) -> deleteModule code
        ("changeYear":year:firstName:lastName:currentYear:_) -> changeYear (read year) [firstName, lastName] (read currentYear)
        ("printStudentInfo":_) -> printStudentInfo
        ("printModuleInfo":code:_) -> printModuleInfo code
        ("help":_) -> help
        _ -> putStrLn "Invalid command. Type \"help\" to see available commands."