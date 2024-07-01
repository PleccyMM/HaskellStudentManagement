{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main (main
            ,searchStudents
            ,addStudent
            ,deleteStudent
            ,enrollInModule
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

enrollInModule :: String -> [String] -> Int -> IO ()
enrollInModule mod i a = do
    d <- getAllModules
    case d of
        Left err -> putStrLn err
        Right mods -> do
            let d' = findCode mods (pack mod)
            case d' of
                Nothing -> putStrLn "Couldn't find module"
                Just m -> do
                    d'' <- getAllStudents
                    case d'' of
                        Left err -> putStrLn err
                        Right students -> do
                            let d''' = findSpecificStudent students (map pack i) a 
                            case d''' of
                                Nothing -> putStrLn "Couldn't find student"
                                Just st -> do
                                        let updatedStudents = remove st students ++ [enrollModule (pack mod) st]
                                        B.writeFile jsonFileStudent (encodePretty updatedStudents)
                                        putStrLn "Successfully enrolled"

addModule :: String -> String -> IO ()
addModule c n = do
        d <- getAllModules
        case d of
            Left err -> putStrLn err
            Right mods -> do
                moduleExist <- checkModuleExistance [c]
                if moduleExist then putStrLn "Duplicate module" else do
                    m <- constructModule c n
                    let updatedModules = m : mods
                    B.writeFile jsonFileModule (encodePretty updatedModules)
                    putStrLn "Successfully added module"

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
                            putStrLn "Successfully deleted module"

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
                    putStrLn "Successfully updated year of study"

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
                        Right st -> hPutStr inputHandle ("Module " ++ s ++ " " ++ unpack (getName mc) ++ " Enrollment:\n\n" ++ c ++ "\nNumber of Students: " ++ show (countNumberOfLines c))
                            where c = checkEnrolled st mc
    hClose inputHandle

help :: IO ()
help = putStrLn ("\nsearchStudents \"<first or last name>\"" ++
                "\naddStudent \"<first name>\" \"<last name>\" <age> <year> \"<module codes seperate by space>\"" ++
                "\ndeleteStudent \"<first name>\" \"<last name>\" <age>" ++ 
                "\nenrollInModule \"<module code>\" \"<first name>\" \"<last name>\" <age>" ++
                "\naddModule \"<code>\" \"<name>\"" ++
                "\ndeleteModule \"<code>\"" ++
                "\nchangeYear <year> \"<first name>\" \"<last name>\" <age>" ++
                "\nprintStudentInfo" ++
                "\nprintModuleInfo \"<code>\"\n")

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("searchStudents":name:_) -> searchStudents name
        ("addStudent":firstName:lastName:age:year:modules:_) -> 
            if isInt age && isInt year
                then addStudent firstName lastName (read age) (read year) modules
                else putStrLn "Ensure that the age and year are entered correctly"
        ("deleteStudent":firstName:lastName:age:_) -> 
            if isInt age 
                then deleteStudent [firstName, lastName] (read age)
                else putStrLn "Ensure that the age is entered correctly"
        ("enrollInModule":code:firstName:lastName:age:_) -> 
            if isInt age 
                then enrollInModule code [firstName, lastName] (read age)
                else putStrLn "Ensure that the age is entered correctly"
        ("addModule":code:name:_) -> addModule code name
        ("deleteModule":code:_) -> deleteModule code
        ("changeYear":year:firstName:lastName:age:_) -> 
            if isInt age  && isInt year
                then changeYear (read year) [firstName, lastName] (read age)
                else putStrLn "Ensure that the age and year are entered correctly"
        ("printStudentInfo":_) -> printStudentInfo
        ("printModuleInfo":code:_) -> printModuleInfo code
        ("help":_) -> help
        _ -> putStrLn "Invalid command. Type \"help\" to see available commands."
