module Process

import System.File.ReadWrite
import JSON.Derive

import Course
import Student

%language ElabReflection

decodefile : Show a => FromJSON a => String -> IO (Either String a)
decodefile filename = do 
  result <- readFile filename
  case result of
       (Left x)  => pure $ Left $ show x
       (Right x) => case decode  x of
                         (Left err) => pure $ Left $ show err
                         (Right y) => pure $ Right y  



dir : String
dir = "/home/george/Prof-Teach/scores/AY2021-2022--2022-sp--Math135-data"

studentsFile : String
studentsFile = dir ++ "/" ++ "2022-05-17T1708_Grades-Sp22-MATH-0135-Real_Analysis_1.json"

courseFile : String
courseFile = dir ++ "/" ++  "2022-sp-Math135.json"

-- dir : String
-- dir = "/home/george/Prof-Teach/scores/AY2021-2022--2022-sp--Math051-data"

-- studentsFile : String
-- studentsFile = dir ++ "/" ++ "2022-05-17T0950_Grades-Sp22-MATH-0051-01-Differential_Equations.json"

-- courseFile : String
-- courseFile = dir ++ "/" ++  "2022-sp-Math051.json"


-- dir : String
-- dir = "/home/george/Prof-Teach/scores/AY2022-2023--2023-sp--Math135-data"

-- studentsFile : String
-- studentsFile = dir ++ "/" ++ "2023-07-24T1613_Grades-Sp23-MATH-0135-01-Real_Analysis_I.json"

-- courseFile : String
-- courseFile = dir ++ "/" ++  "2023-sp-Math135.json"


students : IO (Either String (List StudentSISData))
students = decodefile studentsFile

course : IO (Either String Course)
course = decodefile courseFile

explain : { a: Type} -> (msg : String) -> Maybe a -> Either String a
explain msg Nothing = Left msg
explain msg (Just x) = Right x


computeResults : Either String Course -> Either String (List StudentSISData) -> Either String (List StudentResult)
computeResults ec es = do
  course <- ec
  students <- es
  
  explain errmsg $ traverse (result course) students
  
  where
    errmsg : String
    errmsg = "Failed to construct student results"

report : Either String Course -> Either String (List StudentSISData) -> Either String String
report ec es = do
  course <- ec
  students <- es
  
  case students of
     [] => pure "foo"
     (x :: xs) => pure $ report course x
  


main : IO ()
main = do 
  s <- students
  c <- course
  case computeResults c s of
    (Left err) => putStrLn err
    (Right x) => traverse_ (putStrLn . show) x
  case report c s of
    (Left err) => putStrLn err
    (Right x) => putStrLn x
    
       

