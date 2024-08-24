module CourseData

import Derive.Prelude
import JSON.Simple.Derive
import LetterGrades

%language ElabReflection

--------------------------------------------------------------------------------

singleOptions : Options
singleOptions = { sum := ObjectWithSingleField
--                , constructorTagModifier := toLower 
                , replaceMissingKeysWithNull := True } defaultOptions


myToJSON   = customToJSON   Export singleOptions
myFromJSON = customFromJSON Export singleOptions

--------------------------------------------------------------------------------

public export
data Semester : Type where
  Fall : Semester
  Spring : Semester

public export
record AY where
  constructor MkAY 
  ay : Nat

public export
record AcademicSemester where
  constructor MkAcademicSemester
  ay : AY
  semester : Semester


implementation Show AY where
  show x = show x.ay ++ "-" ++ show (x.ay + 1)


%runElab derive "AY" [Eq,myToJSON,myFromJSON]
%runElab derive "Semester" [Show, Eq, myToJSON, myFromJSON]  
%runElab derive "AcademicSemester" [Eq, myToJSON, myFromJSON]  

public export
implementation Show AcademicSemester where
  show ac = show ac.ay ++ "-" ++ show ac.semester

--------------------------------------------------------------------------------

export 
record Section where
  constructor MkSection
  sectionID : String
  
%runElab derive "Section" [Show, Eq, myToJSON, myFromJSON]  

public export
data Computation : Type where
  Value : String -> Computation
  Max   : String -> String -> Computation
  Min   : String -> String -> Computation
  Maxl  : List String -> Computation
  Minl  : List String -> Computation
                 
%runElab derive "Computation" [ Show, Eq, myToJSON, myFromJSON]

public export
record FormulaComponent where
  constructor MkFormulaComponent
  compName : String
  component: Computation
  weight : Double
    
%runElab derive "FormulaComponent" [ Show, Eq, myToJSON, myFromJSON ]

public export
record Formula where
  constructor MkFormula
  id : String
  formulaComps :  List FormulaComponent

%runElab derive "Formula" [ Show, Eq, myToJSON, myFromJSON ]

public export
record Score where
  constructor MkScore
  scoreName : String
  items : List String
  drops : Maybe Nat
    
%runElab derive "Score" [Show, Eq, myToJSON, myFromJSON]

public export
record StudentException where
  constructor MkStudentException
  name : String
  id : String
  formulas : List String
  incomplete: Bool
  comment: Maybe String
  
%runElab derive "StudentException" [Show, Eq, myToJSON, myFromJSON]  

public export
record Course where
  constructor MkCourse
  title   : String
  instructors : List String
  semester : AcademicSemester
  scores   : List Score
  formulas   : List Formula
  exceptFormulas : List Formula
  sections : List Section
  grades : Maybe (List Grade)
  exceptions: List StudentException
  
%runElab derive "Course" [Show, Eq, myToJSON, myFromJSON]

export
reportFileName : Course -> String
reportFileName course = 
  course.title ++ "-" ++ show course.semester ++ ".md"


export
courseLetterGrades : Course -> List Grade
courseLetterGrades course = case course.grades of
                              Nothing => defaultLetterGrades
                              (Just gt) => gt


--------------------------------------------------------------------------------
-- student

public export 
record Outcome where
  constructor MkOutcome
  scoreName  : String
  marks      : List Double

%runElab derive "Outcome" [ Show, Eq, myToJSON, myFromJSON ]



public export
record StudentData where
  constructor MkStudentData
  name    : String
  id      : String
  section : List String  
  email   : String
  level   : String
  school  : String
  majors  : List String
  outcomes : List Outcome
  courseScore : Maybe Double
  grade       : Maybe Grade

%runElab derive "StudentData" [Show, Eq, myToJSON, myFromJSON]

