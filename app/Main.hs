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
            ) where

import Lib
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import Data.Text hiding (map)
import System.IO

searchStudents :: String -> IO ()
searchStudents s = do 
    d <- findStudents s
    case d of
        Nothing -> putStrLn "Didn't find any students"
        Just st -> putStrLn $ studentToString st

addStudent :: IO ()
addStudent = do
        d <- getAllStudents
        case d of
            Left err -> putStrLn err
            Right students -> do
                s <- getStudentDetails
                let updatedStudents = s : students
                B.writeFile jsonFileStudent (encode updatedStudents)

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
                    B.writeFile jsonFileStudent (encode updatedStudents)
    
addModule :: IO ()
addModule = do
        d <- getAllModules
        case d of
            Left err -> putStrLn err
            Right mods -> do
                m <- getModuleDetails
                let updatedModules = m : mods
                B.writeFile jsonFileModule (encode updatedModules)

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
                            B.writeFile jsonFileStudent (encode updatedStudents)
                            B.writeFile jsonFileModule (encode updatedModules)

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
                    B.writeFile jsonFileStudent (encode updatedStudents)

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

main :: IO ()
main = putStrLn "Welcome to the student management system!"