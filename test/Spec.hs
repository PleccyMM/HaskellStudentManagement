import Test.HUnit
 
main :: IO ()
main = do
        runTestTTAndExit (test [testSearchEmpty
                               ,testSearchRealFirst      
                               ])

testSearchEmpty :: Test
testSearchEmpty = TestCase ( assertEqual "search empty file" expected (findStudents input) )
    where input = []
          expected = Nothing

testSearchRealFirst :: Test
testSearchRealFirst = TestCase ( assertEqual "search for an existing student by first name" expected (findStudents input) )
    where input = "Reuben"
          expected = Just (Student { firstName = "Reuben", secondName = "Shaw", age = 20, year = 2, modules = ["BS2201","BS2202","BS2220","BS2221"] })