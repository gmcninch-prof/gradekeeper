module LetterGrades 

import Derive.Prelude
import JSON.Simple.Derive
import Data.SortedMap
import Util 

%language ElabReflection

singleOptions : Options
singleOptions = { sum := ObjectWithSingleField
                , constructorTagModifier := toLower 
                , replaceMissingKeysWithNull := True } defaultOptions


public export 
data Grade : Type where
  MkGrade : (label : String) -> (min : Double) -> Grade
  Failing : Grade
  Incomplete : Grade

%runElab derive "Grade" [ Eq, customToJSON   Export singleOptions, customFromJSON   Export singleOptions ]

export 
implementation Show Grade where
  show (MkGrade label min) = label
  show Failing = "F"
  show Incomplete = "Inc"


lookupGrade : List Grade -> String -> Maybe Grade
lookupGrade lg g  = 
  lookup g gradeMap
  where
    gradeMap : SortedMap String Grade
    gradeMap = fromList $ (\g => (show g,g)) <$> lg



implementation Ord Grade where
  compare (MkGrade _ min1) (MkGrade _ min2) = compare min1 min2
  compare (MkGrade _ _) _ = GT
  compare _ (MkGrade _ _) = LT
  compare Failing Failing = EQ
  compare Incomplete Incomplete = EQ
  compare Failing Incomplete = GT
  compare Incomplete Failing = LT

export
defaultLetterGrades : List Grade
defaultLetterGrades = [ MkGrade "A+" 98.0
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
                      , Failing
                      , Incomplete
                      ]
              

export               
computeGrade : (letterGrades : List Grade) -> (score : Double) -> Grade
computeGrade letterGrades score = computeGrade' descendingLetterGrades score
  where

    rev : Ord a => a -> a -> Ordering
    rev x y = compare y x

    descendingLetterGrades : List Grade
    descendingLetterGrades = sortBy rev letterGrades
    
    computeGrade' :  (letterGrades : List Grade) -> (score : Double) -> Grade
    computeGrade' [] score  = Failing
    computeGrade' (x :: xs) score = case x of
      t@(MkGrade _ min) => if (round 1 score) >= min then t else computeGrade' xs score 
      Failing => Failing
      Incomplete => Failing

  
