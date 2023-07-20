module Main

import System.File.ReadWrite

--import Data.String
import JSON.Derive

import LetterGrades

%language ElabReflection


data ScoreT : Type where
  Score : ( score : Double ) -> ScoreT
  ListScores : (scores : List Double) -> ScoreT


untaggedOptions : Options
untaggedOptions = { sum := UntaggedValue
                  , constructorTagModifier := toLower } defaultOptions

  
singleOptions : Options
singleOptions = { sum := ObjectWithSingleField
                , constructorTagModifier := toLower } defaultOptions


%runElab derive "ScoreT" [Show, Eq, customToJSON untaggedOptions, customFromJSON untaggedOptions ]

record Result where
  constructor MkResult
  label  : String
  value  : ScoreT

%runElab derive "Result" [Show, Eq, ToJSON, FromJSON]

data ComputeStrategy : Type where
  Average : ComputeStrategy
  DropAndAverage : ( num:Nat) -> ComputeStrategy

%runElab derive "ComputeStrategy" [Show, Eq, customToJSON singleOptions, customFromJSON singleOptions ]

data ScoreComponent : Type where
  Comp : (label:String) -> (weight:Double) ->  ScoreComponent
  Max : (labels : List String) -> (weight:Double) -> ScoreComponent
  Min : (labels : List String) -> (weigth:Double) -> ScoreComponent

%runElab derive "ScoreComponent" [Show, Eq, customToJSON singleOptions, customFromJSON singleOptions ]


record Section where
  constructor MkSection
  sectionID : String
  
%runElab derive "Section" [Show, Eq, ToJSON, FromJSON]  

record Formula where
  constructor MkFormula 
  id : String
  formula :  List (String, ScoreComponent)

%runElab derive "Formula" [Show, Eq, ToJSON, FromJSON]  

record Course where
  constructor MkCourse
  title   : String
  strategies : List (String, ComputeStrategy)
  formulas   : List Formula
  sections : List Section

%runElab derive "Course" [Show, Eq, ToJSON, FromJSON]


record StudentSISData where
  constructor MkStudentSISData
  name    : String
  section : String
  results : List Result

%runElab derive "StudentSISData" [Show, Eq, ToJSON, FromJSON]

record StudentResults where
  constructor MkStudentResults
  name    : String
  section : String
  courseScore : Double
  grade       : Grade


%runElab derive "StudentResults" [Show, Eq, ToJSON, FromJSON]


summarizeGrades : (letterGrades : List Grade) -> (results : List StudentResults) -> List ( Grade, Nat )
summarizeGrades letterGrades results = summarizeGrade' <$> letterGrades
  where
    summarizeGrade' : Grade -> ( Grade, Nat )
    summarizeGrade' grade = 
      (grade, countGrades letterGrades grade (courseScore <$> results))


decodefile : Show a => FromJSON a => String -> IO (Either String a)
decodefile filename = do 
  result <- readFile filename
  case result of
       (Left x)  => pure $ Left $ show x
       (Right x) => case decode  x of
                         (Left err) => pure $ Left $ show err
                         (Right y) => pure $ Right y  


dir : String
dir = "/home/george/Prof-Teach/2022-2023--sp/Math135/scores/"

studentsFile : String
studentsFile = dir ++ "/" ++ "2023-05-12T1027_Grades-Sp23-MATH-0135-01-Real_Analysis_I-alt.json"

courseFile : String
courseFile = dir ++ "/" ++  "2023-sp-Math135.json"


students : IO (Either String (List StudentSISData))
students = decodefile studentsFile

course : IO (Either String Course)
course = decodefile courseFile

main : IO ()
main = do 
  s <- students
  c <- course
  printLn s
  printLn c
  pure ()


