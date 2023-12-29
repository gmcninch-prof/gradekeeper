module LetterGrades 

import Derive.Prelude
import JSON.Derive
import Util 

%language ElabReflection


untaggedOptions : Options
untaggedOptions = { sum := UntaggedValue
                  , constructorTagModifier := toLower } defaultOptions

singleOptions : Options
singleOptions = { sum := ObjectWithSingleField
                , constructorTagModifier := toLower } defaultOptions


public export 
data Grade : Type where
  MkGrade : (label : String) -> (min : Double) -> Grade
  Failing : Grade
  Incomplete : Grade

%runElab derive "Grade" [ Show, Eq, customToJSON untaggedOptions, customFromJSON untaggedOptions ]

implementation Show Grade where
  show (MkGrade label min) = label
  show Failing = "F"
  show Incomplete = "Inc"

implementation Ord Grade where
  compare (MkGrade _ min1) (MkGrade _ min2) = compare min1 min2
  compare (MkGrade _ _) _ = GT
  compare _ (MkGrade _ _) = LT
  compare Failing Failing = EQ
  compare Incomplete Incomplete = EQ
  compare Failing Incomplete = GT
  compare Incomplete Failing = LT

export
letterGrades : List Grade
letterGrades = [ MkGrade "A+" 98.0
               , MkGrade "A"  92.5
               , MkGrade "A-" 89.5
               , MkGrade "B+" 86.5
               , MkGrade "B"  82.5
               , MkGrade "B-" 79.5
               , MkGrade "C+" 76.5
               , MkGrade "C"  72.5
               , MkGrade "C-" 69.5
               , MkGrade "D+" 66.5
               , MkGrade "D"  62.5
               , MkGrade "D-" 59.5
               ]
              

export               
computeGrade : (letterGrades : List Grade) -> (score : Double) -> String
computeGrade letterGrades score = computeGrade' descendingLetterGrades score
  where

    rev : Ord a => a -> a -> Ordering
    rev x y = compare y x

    descendingLetterGrades : List Grade
    descendingLetterGrades = sortBy rev letterGrades
    
    computeGrade' :  (letterGrades : List Grade) -> (score : Double) -> String
    computeGrade' [] score  = show Failing
    computeGrade' (x :: xs) score = case x of
      t@(MkGrade _ min) => if (round 1 score) >= min then show t else computeGrade' xs score 
      Failing => show Failing
      Incomplete => show Incomplete


export
countGrades : (letterGrades : List Grade) -> (grade : Grade) -> (scores : List Double) -> Nat
countGrades letterGrades grade scores = List.length matches
  where
    gradeMatch : Double -> Bool
    gradeMatch score = show grade == computeGrade letterGrades score

    matches : List Double
    matches = filter gradeMatch scores
  
