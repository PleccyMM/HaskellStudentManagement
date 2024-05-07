{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main (main
            ,searchStudents
            ,addStudent
            ,addModule
            ,printStudentInfo
            ,printModuleInfo
            ) where

import Lib
import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as B
import Data.Text
import System.IO

searchStudents :: String -> IO ()
searchStudents s = do 
    result <- findStudents s
    maybe (putStrLn "Didn't find student") print result

addStudent :: IO ()
addStudent = do
        d <- getAllStudents
        case d of
            Left err -> putStrLn err
            Right students -> do
                s <- getStudentDetails
                let updatedStudents = s : students
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