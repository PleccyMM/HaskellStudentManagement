# Haskell Student Directory System

## Running Commands

* Open the command line
* Navigate to the directory where you have the haskell program stored
* Run `stack build`
* From now on you can do `stack run -- commandName commandParameters` to execute commands, all commands and parameters are below
* Alternatively run `stack test` to run all test cases inside the program

## Available Commands

>[!IMPORTANT]
>**All available commands can also be found by doing `stack run -- help`** <br/><br/>
>Ensure you quotation marks are respected for relevant pieces of data input

1. `searchStudents "<first or last name>"` - prints all student information to console
2. `addStudent "<first name>" "<last name>" <age> <year> "<module codes seperate by space>"` - adds a new student to the JSON file, must not share name and age with an existing student
3. `deleteStudent "<first name>" "<last name>" <age>` - deletes the student specified by their name and age in the JSON file
4. `enrollInModule "<module code>" "<first name>" "<last name>" <age>` - enrolls the student specified by their name and age into the module denoted by its code
5. `addModule "<code>" "<name>"` - adds a new module to the module JSON file, must not overlap code with an existing module
6. `deleteModule "<code>"` - removes the module specified by the code from the modules JSON
7. `changeYear <year> "<first name>" "<last name>" <age>` - changes the year of study of the student specified by their name and age to the year provided
8. `printStudentInfo` - prints information about all students to AllStudentInfo.txt in the directory
9. `printModuleInfo "<code>"` - prints all information about the specified module to a file in the directory, that shares the name with the module code
