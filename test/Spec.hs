{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import Lib
import Data.Text hiding (map)

main :: IO ()
main = do
    runTestTTAndExit (test [testFirstName,
                            testSecondName,
                            testEnrolledModules,
                            testSearchMissing,
                            testSearchFirst,
                            testSearchSecond,
                            testSearchAllLower,
                            testSearchAllCapital,
                            testSearchRepeat,
                            testIsIntNumber,
                            testIsIntDecimal,
                            testIsIntText,
                            testGetModuleCode,
                            testFindModuleCodeMissing,
                            testFindModuleCodeExistant,
                            testCheckModuleMissing,
                            testCheckModuleExistant,
                            testCheckModuleCase,
                            testCheckModuleEmptyText,
                            testCheckModuleEmptyModules,
                            testSearchModuleSingleMissing,
                            testSearchModuleMultipleMissing,
                            testSearchModuleSingleExistant,
                            testSearchModuleMultipleExistant,
                            testSearchModuleMultipleMixed,
                            testSearchModuleOverflow,
                            testSearchModuleDuplicate,
                            testSearchModuleEmptyModule,
                            testCheckEnrolledMissing,
                            testCheckEnrolledExistant,
                            testCheckEnrolledEmpty,
                            testLineCountFew,
                            testLineCountMany,
                            testStudentToString,
                            testMultipleStudentToString
                            ])

-- STUDENT DETAILS TESTING --

testFirstName :: Test
testFirstName = TestCase $ do
    let input = exampleRealStudet
        expected = "Reuben"
    let actual = getFirstName input
    assertEqual "checks if getting the first name works as expected" expected actual

testSecondName :: Test
testSecondName = TestCase $ do
    let input = exampleRealStudet
        expected = "Shaw"
    let actual = getSecondName input
    assertEqual "checks if getting the second name works as expected" expected actual

testEnrolledModules :: Test
testEnrolledModules = TestCase $ do
    let input = exampleRealStudet
        expected = ["BS2201","BS2202","BS2220","BS2221"]
    let actual = getStudentModules input
    assertEqual "checks if getting a student's modules works as expected" expected actual

        
-- STUDENT SEARCH TESTING --

testSearchMissing :: Test
testSearchMissing = TestCase $ do
    let input1 = exampleStudentFile
        input2 = "Jake"
        expected = []
    let actual = checkTextStudent input1 input2
    assertEqual "search for a missing student" expected actual

testSearchFirst :: Test
testSearchFirst = TestCase $ do
    let input1 = exampleStudentFile
        input2 = "Reuben"
        expected = [exampleRealStudet]
    let actual = checkTextStudent input1 input2
    assertEqual "search for an existing students first name" expected actual
    
testSearchSecond :: Test
testSearchSecond = TestCase $ do
    let input1 = exampleStudentFile
        input2 = "Shaw"
        expected = [exampleRealStudet]
    let actual = checkTextStudent input1 input2
    assertEqual "search for an existing students second name" expected actual
    
testSearchAllLower :: Test
testSearchAllLower = TestCase $ do
    let input1 = exampleStudentFile
        input2 = "reuben"
        expected = [exampleRealStudet]
    let actual = checkTextStudent input1 input2
    assertEqual "search for an existing students in lowercase" expected actual
    
testSearchAllCapital :: Test
testSearchAllCapital = TestCase $ do
    let input1 = exampleStudentFile
        input2 = "REUBEN"
        expected = [exampleRealStudet]
    let actual = checkTextStudent input1 input2
    assertEqual "search for an existing students in uppercase" expected actual

testSearchRepeat :: Test
testSearchRepeat = TestCase $ do
    let input1 = exampleStudentFile
        input2 = "Vanessa"
        expected = [(Student { 
            firstName = "Vanessa", 
            secondName = "Thompson", 
            age = 24, 
            year = 3, 
            modules = ["BS3303","BS3312","BS33333","BS2220"] }),
            (Student { 
            firstName = "Vanessa", 
            secondName = "Richards", 
            age = 22, 
            year = 1, 
            modules = ["BS1001","BS1101","BS1112"] })]
    let actual = checkTextStudent input1 input2
    assertEqual "search for students with the same name" expected actual

-- STRING IS INT TESTING --

testIsIntNumber :: Test
testIsIntNumber = TestCase $ do
    let input = "23"
        expected = True
    let actual = isInt input
    assertEqual "check an actual integer is an integer" expected actual
    
testIsIntDecimal :: Test
testIsIntDecimal = TestCase $ do
    let input = "23.5"
        expected = False
    let actual = isInt input
    assertEqual "checks if a decimal is an integer" expected actual
        
testIsIntText :: Test
testIsIntText = TestCase $ do
    let input = "test"
        expected = False
    let actual = isInt input
    assertEqual "checks if a string of text is an integer" expected actual

-- MODULE CODE TESTING --
        
testGetModuleCode :: Test
testGetModuleCode = TestCase $ do
    let input = exampleRealModule
        expected = "BS2220"
    let actual = getCode input
    assertEqual "checks if getting module codes works as expected" expected actual
        
testFindModuleCodeMissing :: Test
testFindModuleCodeMissing = TestCase $ do
    let input1 = exampleModuleFile
        input2 = pack "MD0001"
        expected = Nothing
    let actual = findCode input1 input2
    assertEqual "checks if getting module codes works as expected by checking for a missing module" expected actual

testFindModuleCodeExistant :: Test
testFindModuleCodeExistant = TestCase $ do
    let input1 = exampleModuleFile
        input2 = pack "BS2220"
        expected = Just exampleRealModule
    let actual = findCode input1 input2
    assertEqual "checks if getting module codes works as expected by checking for an existant module" expected actual

-- CHECK MODULE TESTING (NOT SEARCHING) --

testCheckModuleMissing :: Test
testCheckModuleMissing = TestCase $ do
    let input1 = pack "MD0001"
        input2 = exampleModuleFile
        expected = False
    let actual = checkModules input2 input1
    assertEqual "check for a missing module" expected actual

testCheckModuleExistant :: Test
testCheckModuleExistant = TestCase $ do
    let input1 = pack "BS2220"
        input2 = exampleModuleFile
        expected = True
    let actual = checkModules input2 input1
    assertEqual "check for an existing module" expected actual

testCheckModuleCase :: Test
testCheckModuleCase = TestCase $ do
    let input1 = pack "bs2220"
        input2 = exampleModuleFile
        expected = True
    let actual = checkModules input2 input1
    assertEqual "check for an existing module with the wrong case" expected actual

testCheckModuleEmptyText :: Test
testCheckModuleEmptyText = TestCase $ do
    let input1 = pack ""
        input2 = exampleModuleFile
        expected = False
    let actual = checkModules input2 input1
    assertEqual "check for a module with no input" expected actual

testCheckModuleEmptyModules :: Test
testCheckModuleEmptyModules = TestCase $ do
    let input1 = pack "BS2220"
        input2 = []
        expected = False
    let actual = checkModules input2 input1
    assertEqual "check for a module with no input" expected actual

-- SEARCH MODULE TESTING --

testSearchModuleSingleMissing :: Test
testSearchModuleSingleMissing = TestCase $ do
    let input1 = map pack ["MD0001"]
        input2 = exampleModuleFile
        expected = False
    let actual = searchModules input1 input2
    assertEqual "search for a single missing modules" expected actual

testSearchModuleMultipleMissing :: Test
testSearchModuleMultipleMissing = TestCase $ do
    let input1 = map pack ["MD0001", "MD0002", "MD0003"]
        input2 = exampleModuleFile
        expected = False
    let actual = searchModules input1 input2
    assertEqual "search for multiple missing modules" expected actual

testSearchModuleSingleExistant :: Test
testSearchModuleSingleExistant = TestCase $ do
    let input1 = map pack ["BS2220"]
        input2 = exampleModuleFile
        expected = True
    let actual = searchModules input1 input2
    assertEqual "search for a single existing module" expected actual

testSearchModuleMultipleExistant :: Test
testSearchModuleMultipleExistant = TestCase $ do
    let input1 = map pack ["BS2220", "BS2202", "BS2221"]
        input2 = exampleModuleFile
        expected = True
    let actual = searchModules input1 input2
    assertEqual "search for multiple existing modules" expected actual

testSearchModuleMultipleMixed :: Test
testSearchModuleMultipleMixed = TestCase $ do
    let input1 = map pack ["BS2220", "MD0001", "BS2221"]
        input2 = exampleModuleFile
        expected = False
    let actual = searchModules input1 input2
    assertEqual "search for multiple mixed existing and missing modules" expected actual

testSearchModuleOverflow :: Test
testSearchModuleOverflow = TestCase $ do
    let input1 = map pack ["BS2220", "BS2221", "BS2202", "BS2203", "BS2201", "BS2222"]
        input2 = exampleModuleFile
        expected = False
    let actual = searchModules input1 input2
    assertEqual "search for a module provided with more text than modules" expected actual

testSearchModuleDuplicate :: Test
testSearchModuleDuplicate = TestCase $ do
    let input1 = map pack ["BS2220", "BS2220"]
        input2 = exampleModuleFile
        expected = True
    let actual = searchModules input1 input2
    assertEqual "search for a module provided with the same module code twice" expected actual

testSearchModuleEmptyModule :: Test
testSearchModuleEmptyModule = TestCase $ do
    let input1 = map pack ["BS2220"]
        input2 = []
        expected = False
    let actual = searchModules input1 input2
    assertEqual "search for a module without providing any modules" expected actual

-- CHECK ENROLLED TESTING --

testCheckEnrolledMissing :: Test
testCheckEnrolledMissing = TestCase $ do
    let input1 = exampleStudentFile
        input2 = (Module {code = "MD0001", name = "Media Intro"})
        expected = ""
    let actual = checkEnrolled input1 input2
    assertEqual "tests getting all student names who are enrolled on a missing module" expected actual

testCheckEnrolledExistant :: Test
testCheckEnrolledExistant = TestCase $ do
    let input1 = exampleStudentFile
        input2 = exampleRealModule
        expected = "Reuben Shaw\nVanessa Thompson\n"
    let actual = checkEnrolled input1 input2
    assertEqual "tests getting all student names who are enrolled on a specific module" expected actual

testCheckEnrolledEmpty :: Test
testCheckEnrolledEmpty = TestCase $ do
    let input1 = []
        input2 = exampleRealModule
        expected = ""
    let actual = checkEnrolled input1 input2
    assertEqual "tests getting all student names who are enrolled on a specific module when no students are provided" expected actual

-- STUDENT LINE COUNTER TESTING --

testLineCountFew :: Test
testLineCountFew = TestCase $ do
    let input = "1\n2\n3\n"
        expected = 3
    let actual = countNumberOfLines input
    assertEqual "checks if line counting codes works as expected with 3 lines" expected actual

testLineCountMany :: Test
testLineCountMany = TestCase $ do
    let input = "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n"
        expected = 13
    let actual = countNumberOfLines input
    assertEqual "checks if line counting codes works as expected with 13 lines" expected actual

-- STUDENT TO STRING TESTING --

testStudentToString :: Test
testStudentToString = TestCase $ do
    let input = exampleRealStudet
        expected = "First Name: Reuben\nSecond Name: Shaw\nAge: 20\nYear of Study: 2\nModules: BS2201 BS2202 BS2220 BS2221"
    let actual = studentToString input
    assertEqual "tests conversion from student type to readable string" expected actual

testMultipleStudentToString :: Test
testMultipleStudentToString = TestCase $ do
    let input = exampleStudentFile
        expected = "\nFirst Name: Reuben\nSecond Name: Shaw\nAge: 20\nYear of Study: 2\nModules: BS2201 BS2202 BS2220 BS2221\n\n" ++
                    "First Name: Amy\nSecond Name: Lovegood\nAge: 23\nYear of Study: 1\nModules: BS1001 BS1101 BS1112\n\n" ++
                    "First Name: Vanessa\nSecond Name: Thompson\nAge: 24\nYear of Study: 3\nModules: BS3303 BS3312 BS33333 BS2220\n\n" ++
                    "First Name: Vanessa\nSecond Name: Richards\nAge: 22\nYear of Study: 1\nModules: BS1001 BS1101 BS1112\n"
    let actual = convertAllStudents input
    assertEqual "tests conversion from student type to readable string" expected actual

exampleStudentFile :: [Student]
exampleStudentFile = [(Student { 
            firstName = "Reuben", 
            secondName = "Shaw", 
            age = 20, 
            year = 2, 
            modules = ["BS2201","BS2202","BS2220","BS2221"] }),
            (Student { 
            firstName = "Amy", 
            secondName = "Lovegood", 
            age = 23, 
            year = 1, 
            modules = ["BS1001","BS1101","BS1112"] }),
            (Student { 
            firstName = "Vanessa", 
            secondName = "Thompson", 
            age = 24, 
            year = 3, 
            modules = ["BS3303","BS3312","BS33333","BS2220"] }),
            (Student { 
            firstName = "Vanessa", 
            secondName = "Richards", 
            age = 22, 
            year = 1, 
            modules = ["BS1001","BS1101","BS1112"] })]

exampleRealStudet :: Student
exampleRealStudet = (Student { 
            firstName = "Reuben", 
            secondName = "Shaw", 
            age = 20, 
            year = 2, 
            modules = ["BS2201","BS2202","BS2220","BS2221"] })

exampleModuleFile :: [Module]
exampleModuleFile = [(Module {code = "BS2221", name = "Discrete Mathematics"}),
                (Module {code = "BS2220", name = "Functional Programming"}),
                (Module {code = "BS2202", name = "Object Oriented Software Development"}),
                (Module {code = "BS2203", name = "Secure Systems Architectures"}),
                (Module {code = "BS2201", name = "Intergrated Project"})]

exampleRealModule :: Module
exampleRealModule = (Module {code = "BS2220", name = "Functional Programming"})