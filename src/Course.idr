module Course 

import Derive.Prelude
import JSON.Derive
import LetterGrades

%language ElabReflection

untaggedOptions : Options
untaggedOptions = { sum := UntaggedValue
                  , constructorTagModifier := toLower } defaultOptions

  
singleOptions : Options
singleOptions = { sum := ObjectWithSingleField
                , constructorTagModifier := toLower } defaultOptions

export 
record Section where
  constructor MkSection
  sectionID : String
  
%runElab derive "Section" [Show, Eq, ToJSON, FromJSON]  


public export
data AvgMethod : Type where
  Average : AvgMethod
  DropAndAverage : ( num:Nat) -> AvgMethod

public export 
record ComputeStrategy where
  constructor MkComputeStrategy
  label : String
  method : AvgMethod

%runElab derive "AvgMethod" [ Show, Eq, customToJSON singleOptions, customFromJSON singleOptions ]
%runElab derive "ComputeStrategy" [ Show, Eq, ToJSON , FromJSON ]

public export
data Computation = Copy String
                 | Max (List String)
                 | Min (List String)
                 
%runElab derive "Computation" [ Show, Eq, customToJSON singleOptions, customFromJSON singleOptions]

public export
record ScoreComponent where
  constructor MkScoreComponent
  compName : String
  computation: Computation
  weight : Double
  
  
%runElab derive "ScoreComponent" [ Show, Eq, customToJSON singleOptions, customFromJSON singleOptions ]

public export
record Formula where
  constructor MkFormula
  id : String
  formula :  List ScoreComponent

%runElab derive "Formula" [Show, Eq, ToJSON, FromJSON]  



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


%runElab derive "AY" [Eq,ToJSON,FromJSON]
%runElab derive "Semester" [Show, Eq, ToJSON, FromJSON]  
%runElab derive "AcademicSemester" [Eq, ToJSON, FromJSON]  

public export
implementation Show AcademicSemester where
  show ac = show ac.ay ++ "-" ++ show ac.semester

-- public export
-- data CourseStatus : Type where
--   Finished : CourseStatus
--   Current  : CourseStatus

-- %runElab derive "CourseStatus" [ Show, Eq, customToJSON singleOptions, customFromJSON singleOptions ]


public export
record Course where
  constructor MkCourse
  title   : String
--  status : CourseStatus
  instructors : List String
  semester : AcademicSemester
  strategies : List ComputeStrategy
  formulas   : List Formula
  gradingFormulas : List String
  sections : List Section
  grades : Maybe (List Grade)
  exceptionsFile: Maybe String
  
%runElab derive "Course" [Show, Eq, ToJSON, FromJSON]

export
reportFileName : Course -> String
reportFileName course = 
  course.title ++ "-" ++ show course.semester ++ ".md"
