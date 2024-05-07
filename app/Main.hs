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
import System.IO

searchStudents :: String -> IO ()
searchStudents s = do 
    result <- findStudents s
    maybe (Prelude.putStrLn "Didn't find student") print result

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

printStudentInfo :: IO ()
printStudentInfo = do
    inputHandle <- openFile "AllStudentInfo.txt" WriteMode
    d <- getAllStudents
    case d of
        Left err -> Prelude.putStrLn err
        Right st -> hPutStr inputHandle $ convertAllStudents st
    hClose inputHandle

printModuleInfo :: String -> IO ()
printModuleInfo s = do
    inputHandle <- openFile (s ++ "ModuleInfo.txt") WriteMode
    d <- getAllModules
    case d of
        Left err -> Prelude.putStrLn err
        Right ms -> do
            let m = findCode ms $ pack s
            case m of
                Nothing -> Prelude.putStrLn "Didn't find module code"
                Just mc -> do
                    d' <- getAllStudents
                    case d' of
                        Left err -> Prelude.putStrLn err
                        Right st -> hPutStr inputHandle ("Module " ++ s ++ " Enrollment:\n\n" ++ c ++ "\nNumber of Students: " ++ show (countNumberOfLines c))
                            where c = checkEnrolled st mc
    hClose inputHandle

main :: IO ()
main = Prelude.putStrLn "Welcome to the student management system!"