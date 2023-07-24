module Course 

import Derive.Prelude
import JSON.Derive
import LetterGrades

%language ElabReflection

  
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
data ScoreComponent : Type where
  Copy : (compName : String) -> (label  : String)      -> (weight:Double) -> ScoreComponent
  Max  : (compName : String) -> (labels : List String) -> (weight:Double) -> ScoreComponent
  Min  : (compName : String) -> (labels : List String) -> (weight:Double) -> ScoreComponent

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

implementation Show AY where
  show x = show x.ay ++ "-" ++ show (x.ay + 1)

public export
record AcademicSemester where
  constructor MkAcademicSemester
  ay : AY
  semester : Semester

%runElab derive "AY" [Eq,ToJSON,FromJSON]
%runElab derive "Semester" [Show, Eq, ToJSON, FromJSON]  
%runElab derive "AcademicSemester" [Show, Eq, ToJSON, FromJSON]  

public export
record Course where
  constructor MkCourse
  title   : String
  semester : AcademicSemester
  strategies : List ComputeStrategy
  formulas   : List Formula
  sections : List Section
  grades : Maybe (List Grade)

%runElab derive "Course" [Show, Eq, ToJSON, FromJSON]

