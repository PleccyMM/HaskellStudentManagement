{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit
import StudentDir
import Data.Text hiding (map)

main :: IO ()
main = do
    runTestTTAndExit (test [testFirstName,
                            testSecondName,
                            testAge,
                            testEnrolledModules,
                            testSetStudentModules,
                            testIncreaseYear,
                            testSearchMissing,
                            testSearchFirst,
                            testSearchSecond,
                            testSearchAllLower,
                            testSearchAllCapital,
                            testSearchRepeat,
                            testSpecificSearchMissing,
                            testSpecificSearchExistent,
                            testSpecificSearchEmptyText,
                            testSpecificSearchExistentCase,
                            testIsIntNumber,
                            testIsIntDecimal,
                            testIsIntText,
                            testGetModuleCode,
                            testFindModuleCodeMissing,
                            testFindModuleCodeExistent,
                            testCheckModuleMissing,
                            testCheckModuleExistent,
                            testCheckModuleCase,
                            testCheckModuleEmptyText,
                            testCheckModuleEmptyModules,
                            testSearchModuleSingleMissing,
                            testSearchModuleMultipleMissing,
                            testSearchModuleSingleExistent,
                            testSearchModuleMultipleExistent,
                            testSearchModuleMultipleMixed,
                            testSearchModuleOverflow,
                            testSearchModuleDuplicate,
                            testSearchModuleEmptyModule,
                            testStudentOverlapIdentical,
                            testStudentOverlapRepeat,
                            testStudentOverlapSimilarFirstName,
                            testStudentOverlapSimilarSecondName,
                            testStudentOverlapSimilarBothNames,
                            testStudentOverlapSimilarAge,
                            testStudentOverlapDifferent,
                            testCheckEnrolledMissing,
                            testCheckEnrolledExistent,
                            testCheckEnrolledEmpty,
                            testClearModuleMissing,
                            testClearModuleExistent,
                            testClearModuleEmpty,
                            testLineCountFew,
                            testLineCountMany,
                            testStudentToString,
                            testMultipleStudentToString,
                            testRemoveMissing,
                            testRemoveExistent,
                            testRemoveExistentRepeat
                            ])

-- STUDENT DETAILS TESTING --

testFirstName :: Test
testFirstName = TestCase $ do
    let input = exampleRealStudent
        expected = "Daniel"
    let actual = getFirstName input
    assertEqual "checks if getting the first name works as expected" expected actual

testSecondName :: Test
testSecondName = TestCase $ do
    let input = exampleRealStudent
        expected = "Blain"
    let actual = getSecondName input
    assertEqual "checks if getting the second name works as expected" expected actual

testAge :: Test
testAge = TestCase $ do
    let input = exampleRealStudent
        expected = 20
    let actual = getAge input
    assertEqual "checks if getting a student's age works as expected" expected actual

testEnrolledModules :: Test
testEnrolledModules = TestCase $ do
    let input = exampleRealStudent
        expected = ["BS2201","BS2202","BS2220","BS2221"]
    let actual = getStudentModules input
    assertEqual "checks if getting a student's modules works as expected" expected actual

testSetStudentModules :: Test
testSetStudentModules = TestCase $ do
    let input1 = ["MD0001", "MD0002"]
        input2 = exampleRealStudent
        expected = (Student {firstName = "Daniel", secondName = "Blain", age = 20, year = 2, modules = ["MD0001", "MD0002"]})
    let actual = setStudentModules input1 input2
    assertEqual "checks if setting a student's enrolled modules works as expected" expected actual

testIncreaseYear :: Test
testIncreaseYear = TestCase $ do
    let input1 = exampleRealStudent
        input2 = 3
        expected = (Student {firstName = "Daniel", secondName = "Blain", age = 20, year = 3, modules = ["BS2201","BS2202","BS2220","BS2221"]})
    let actual = increaseYear input1 input2
    assertEqual "checks if setting a student's year of study works as expected" expected actual

        
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
        input2 = "Daniel"
        expected = [exampleRealStudent]
    let actual = checkTextStudent input1 input2
    assertEqual "search for an existing students first name" expected actual
    
testSearchSecond :: Test
testSearchSecond = TestCase $ do
    let input1 = exampleStudentFile
        input2 = "Blain"
        expected = [exampleRealStudent]
    let actual = checkTextStudent input1 input2
    assertEqual "search for an existing students second name" expected actual
    
testSearchAllLower :: Test
testSearchAllLower = TestCase $ do
    let input1 = exampleStudentFile
        input2 = "daniel"
        expected = [exampleRealStudent]
    let actual = checkTextStudent input1 input2
    assertEqual "search for an existing students in lowercase" expected actual
    
testSearchAllCapital :: Test
testSearchAllCapital = TestCase $ do
    let input1 = exampleStudentFile
        input2 = "DANIEL"
        expected = [exampleRealStudent]
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

-- STUDENT SPECIFIC SEARCH TESTING --

testSpecificSearchMissing :: Test
testSpecificSearchMissing = TestCase $ do
    let input1 = exampleStudentFile
        input2 = ["Lucas", "Hall"]
        input3 = 27
        expected = Nothing
    let actual = findSpecificStudent input1 input2 input3
    assertEqual "search for a specific missing student" expected actual

testSpecificSearchExistent :: Test
testSpecificSearchExistent = TestCase $ do
    let input1 = exampleStudentFile
        input2 = ["Daniel", "Blain"]
        input3 = 20
        expected = Just exampleRealStudent
    let actual = findSpecificStudent input1 input2 input3
    assertEqual "search for a specific existing student" expected actual

testSpecificSearchEmptyText :: Test --HEAD GIVES ERROR AS LIST EMPTY
testSpecificSearchEmptyText = TestCase $ do
    let input1 = exampleStudentFile
        input2 = []
        input3 = 20
        expected = Nothing
    let actual = findSpecificStudent input1 input2 input3
    assertEqual "search for a specific student without providing a name" expected actual

testSpecificSearchExistentCase :: Test
testSpecificSearchExistentCase = TestCase $ do
    let input1 = exampleStudentFile
        input2 = ["DANIEL", "BLAIN"]
        input3 = 20
        expected = Just exampleRealStudent
    let actual = findSpecificStudent input1 input2 input3
    assertEqual "search for a specific existing student all in uppercase" expected actual

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

testFindModuleCodeExistent :: Test
testFindModuleCodeExistent = TestCase $ do
    let input1 = exampleModuleFile
        input2 = pack "BS2220"
        expected = Just exampleRealModule
    let actual = findCode input1 input2
    assertEqual "checks if getting module codes works as expected by checking for an existnt module" expected actual

-- CHECK MODULE TESTING (NOT SEARCHING) --

testCheckModuleMissing :: Test
testCheckModuleMissing = TestCase $ do
    let input1 = pack "MD0001"
        input2 = exampleModuleFile
        expected = False
    let actual = checkModules input2 input1
    assertEqual "check for a missing module" expected actual

testCheckModuleExistent :: Test
testCheckModuleExistent = TestCase $ do
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

testSearchModuleSingleExistent :: Test
testSearchModuleSingleExistent = TestCase $ do
    let input1 = map pack ["BS2220"]
        input2 = exampleModuleFile
        expected = True
    let actual = searchModules input1 input2
    assertEqual "search for a single existing module" expected actual

testSearchModuleMultipleExistent :: Test
testSearchModuleMultipleExistent = TestCase $ do
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

-- OVERLAP TESTING STUDENT --

testStudentOverlapIdentical :: Test
testStudentOverlapIdentical = TestCase $ do
    let input1 = exampleRealStudent
        input2 = exampleStudentFile
        expected = True
    let actual = checkStudentOverlap input1 input2
    assertEqual "test to see if an exact copy of a student is identified as a copy" expected actual

testStudentOverlapRepeat :: Test
testStudentOverlapRepeat = TestCase $ do
    let input1 = (Student {firstName = "Daniel", secondName = "Blain", age = 20, year = 3, modules = ["BS2202", "BS2002"]})
        input2 = exampleStudentFile
        expected = True
    let actual = checkStudentOverlap input1 input2
    assertEqual "test to see if a repeat with critical data a student is identified as a copy" expected actual

testStudentOverlapSimilarFirstName :: Test
testStudentOverlapSimilarFirstName = TestCase $ do
    let input1 = (Student {firstName = "Daniel", secondName = "Hall", age = 24, year = 3, modules = ["BS2202", "BS2002"]})
        input2 = exampleStudentFile
        expected = False
    let actual = checkStudentOverlap input1 input2
    assertEqual "test to see if a repeat student with the same first name but different age and second name is identified as a copy" expected actual

testStudentOverlapSimilarSecondName :: Test
testStudentOverlapSimilarSecondName = TestCase $ do
    let input1 = (Student {firstName = "Lucas", secondName = "Blain", age = 24, year = 3, modules = ["BS2202", "BS2002"]})
        input2 = exampleStudentFile
        expected = False
    let actual = checkStudentOverlap input1 input2
    assertEqual "test to see if a repeat student with the same second name but different age and first name is identified as a copy" expected actual

testStudentOverlapSimilarBothNames :: Test
testStudentOverlapSimilarBothNames = TestCase $ do
    let input1 = (Student {firstName = "Daniel", secondName = "Blain", age = 24, year = 3, modules = ["BS2202", "BS2002"]})
        input2 = exampleStudentFile
        expected = False
    let actual = checkStudentOverlap input1 input2
    assertEqual "test to see if a repeat student with the same name but different age is identified as a copy" expected actual

testStudentOverlapSimilarAge :: Test
testStudentOverlapSimilarAge = TestCase $ do
    let input1 = (Student {firstName = "Lucas", secondName = "Hall", age = 20, year = 3, modules = ["BS2202", "BS2002"]})
        input2 = exampleStudentFile
        expected = False
    let actual = checkStudentOverlap input1 input2
    assertEqual "test to see if a repeat student with the same age but different name is identified as a copy" expected actual

testStudentOverlapDifferent :: Test
testStudentOverlapDifferent = TestCase $ do
    let input1 = (Student {firstName = "Robert", secondName = "Daniels", age = 38, year = 3, modules = ["BS2202", "BS2002"]})
        input2 = exampleStudentFile
        expected = False
    let actual = checkStudentOverlap input1 input2
    assertEqual "test to see if an entirely unique student is identified as a copy" expected actual

-- CHECK ENROLLED TESTING --

testCheckEnrolledMissing :: Test
testCheckEnrolledMissing = TestCase $ do
    let input1 = exampleStudentFile
        input2 = (Module {code = "MD0001", name = "Media Intro"})
        expected = ""
    let actual = checkEnrolled input1 input2
    assertEqual "tests getting all student names who are enrolled on a missing module" expected actual

testCheckEnrolledExistent :: Test
testCheckEnrolledExistent = TestCase $ do
    let input1 = exampleStudentFile
        input2 = exampleRealModule
        expected = "Daniel Blain\nVanessa Thompson\n"
    let actual = checkEnrolled input1 input2
    assertEqual "tests getting all student names who are enrolled on a specific module" expected actual

testCheckEnrolledEmpty :: Test
testCheckEnrolledEmpty = TestCase $ do
    let input1 = []
        input2 = exampleRealModule
        expected = ""
    let actual = checkEnrolled input1 input2
    assertEqual "tests getting all student names who are enrolled on a specific module when no students are provided" expected actual

-- CLEAR MODULES TESTING --

testClearModuleMissing :: Test
testClearModuleMissing = TestCase $ do
    let input1 = pack "MD0001"
        input2 = exampleStudentFile
        expected = exampleStudentFile
    let actual = clearModuleFromStudents input1 input2
    assertEqual "tests clearing a module from all students that is missing" expected actual

testClearModuleExistent :: Test
testClearModuleExistent = TestCase $ do
    let input1 = pack "BS2220"
        input2 = exampleStudentFile
        expected = [(Student { 
            firstName = "Daniel", 
            secondName = "Blain", 
            age = 20, 
            year = 2, 
            modules = ["BS2201","BS2202","BS2221"] }),
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
            modules = ["BS3303","BS3312","BS33333"] }),
            (Student { 
            firstName = "Vanessa", 
            secondName = "Richards", 
            age = 22, 
            year = 1, 
            modules = ["BS1001","BS1101","BS1112"] })]
    let actual = clearModuleFromStudents input1 input2
    assertEqual "tests clearing a module from all students that is existent" expected actual

testClearModuleEmpty :: Test
testClearModuleEmpty = TestCase $ do
    let input1 = pack "BS2220"
        input2 = []
        expected = []
    let actual = clearModuleFromStudents input1 input2
    assertEqual "tests clearing a module when no students are provided" expected actual

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
    let input = exampleRealStudent
        expected = "First Name: Daniel\nSecond Name: Blain\nAge: 20\nYear of Study: 2\nModules: BS2201 BS2202 BS2220 BS2221"
    let actual = studentToString input
    assertEqual "tests conversion from student type to readable string" expected actual

testMultipleStudentToString :: Test
testMultipleStudentToString = TestCase $ do
    let input = exampleStudentFile
        expected = "\nFirst Name: Daniel\nSecond Name: Blain\nAge: 20\nYear of Study: 2\nModules: BS2201 BS2202 BS2220 BS2221\n\n" ++
                    "First Name: Amy\nSecond Name: Lovegood\nAge: 23\nYear of Study: 1\nModules: BS1001 BS1101 BS1112\n\n" ++
                    "First Name: Vanessa\nSecond Name: Thompson\nAge: 24\nYear of Study: 3\nModules: BS3303 BS3312 BS33333 BS2220\n\n" ++
                    "First Name: Vanessa\nSecond Name: Richards\nAge: 22\nYear of Study: 1\nModules: BS1001 BS1101 BS1112\n"
    let actual = convertAllStudents input
    assertEqual "tests conversion from student type to readable string" expected actual

-- REMOVE TESTING --

testRemoveMissing :: Test
testRemoveMissing = TestCase $ do
    let input1 = 2
        input2 = exampleIntegerList
        expected = exampleIntegerList
    let actual = remove input1 input2
    assertEqual "tests removing from a list that doesn't have the value" expected actual

testRemoveExistent :: Test
testRemoveExistent = TestCase $ do
    let input1 = "fo"
        input2 = exampleStringList
        expected = ["fi", "do", "lo", "ho", "so"]
    let actual = remove input1 input2
    assertEqual "tests removing from a list that has the value" expected actual

testRemoveExistentRepeat :: Test
testRemoveExistentRepeat = TestCase $ do
    let input1 = 1
        input2 = exampleIntegerList
        expected = [3, 7, 8, 0, 4]
    let actual = remove input1 input2
    assertEqual "tests removing from a list that has the value multiple times" expected actual

-- EXAMPLE DATA --

exampleStudentFile :: [Student]
exampleStudentFile = [(Student { 
            firstName = "Daniel", 
            secondName = "Blain", 
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

exampleRealStudent :: Student
exampleRealStudent = (Student { 
            firstName = "Daniel", 
            secondName = "Blain", 
            age = 20, 
            year = 2, 
            modules = ["BS2201","BS2202","BS2220","BS2221"] })

exampleModuleFile :: [Module]
exampleModuleFile = [(Module {code = "BS2221", name = "Discrete Mathematics"}),
                (Module {code = "BS2220", name = "Functional Programming"}),
                (Module {code = "BS2202", name = "Object Oriented Software Development"}),
                (Module {code = "BS2203", name = "Secure Systems Architectures"}),
                (Module {code = "BS2201", name = "Integrated Project"})]

exampleRealModule :: Module
exampleRealModule = (Module {code = "BS2220", name = "Functional Programming"})

exampleIntegerList :: [Int]
exampleIntegerList = [3, 7, 1, 8, 0, 1, 4]

exampleStringList :: [String]
exampleStringList = ["fi", "fo", "do", "lo", "ho", "so"]