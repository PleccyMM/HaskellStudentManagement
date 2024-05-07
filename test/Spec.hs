{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Lib

main :: IO ()
main = do
    runTestTTAndExit (test [testSearchEmpty
                            ,testSearchRealFirst      
                            ])

testSearchEmpty :: Test
testSearchEmpty = TestCase ( assertEqual "search empty file" expected (findStudents input) )
    where input = ""
        expected = testFunc Nothing

testSearchRealFirst :: Test
testSearchRealFirst = TestCase ( assertEqual "search for an existing student by first name" expected (findStudents input) )
    where input = "Reuben"
        expected = testFunc (Just (Student { firstName = "Reuben", secondName = "Shaw", age = 20, year = 2, modules = ["BS2201","BS2202","BS2220","BS2221"] }))

testFunc :: (Show a) => a -> IO a
testFunc i = do
    r <- i
    return (r)