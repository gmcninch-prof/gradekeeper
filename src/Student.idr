module Student

import JSON.Derive
import Data.SortedMap

import LetterGrades
import Course
import Util

%language ElabReflection

public export 
data ScoreT : Type where
  Score : ( score : Double ) -> ScoreT
  ListScores : (scores : List Double) -> ScoreT


untaggedOptions : Options
untaggedOptions = { sum := UntaggedValue
                  , constructorTagModifier := toLower } defaultOptions

  
singleOptions : Options
singleOptions = { sum := ObjectWithSingleField
                , constructorTagModifier := toLower } defaultOptions


public export 
displayRawScore : ScoreT -> String
displayRawScore (Score score) = show $ round 2 score
displayRawScore (ListScores scores) = show $ (round 2) <$> scores


%runElab derive "ScoreT" [Show, Eq, customToJSON untaggedOptions, customFromJSON untaggedOptions ]

public export 
record Outcome where
  constructor MkOutcome
  label  : String
  value  : ScoreT

%runElab derive "Outcome" [Show, Eq, ToJSON, FromJSON]


public export
record StudentDetails where
  constructor MkStudentDetails
  email : Maybe String
  majors : Maybe String
  level : Maybe String 
  school : Maybe String

%runElab derive "StudentDetails" [ Show, Eq, ToJSON, FromJSON ]

public export
record StudentData where
  constructor MkStudentData
  name    : String
  id      : String
  section : List String
  inSIS   : Bool  
  details : StudentDetails
  outcomes : List Outcome

%runElab derive "StudentData" [Show, Eq, ToJSON, FromJSON]

public export
record StudentResult where
  constructor MkStudentResult
  name    : String
  id      : String
  section : List String
  details : StudentDetails
  courseScore : Double
  grade       : String
  outcomes    : List Outcome

%runElab derive "StudentResult" [Show, Eq, ToJSON, FromJSON]


public export
record StudentException where
  constructor MkStudentException
  name : String
  id : String
  formulas : List String
  incomplete: Bool
  comment: Maybe String
  
%runElab derive "StudentException" [Show, Eq, ToJSON, FromJSON]  
  
