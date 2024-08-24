module Stats

import CourseData
import Data.Maybe
import Data.SortedMap
import Data.String 
import Data.Nat
import Data.Vect
import Data.Vect.Sort
import Data.List1
import Control.Ord

import LetterGrades
import Util

export
gradeMatch : (grade : Grade) -> (student : StudentData) -> Bool
gradeMatch grade student = 
  Just grade == student.grade

export
countMatchingGrades : List StudentData -> (grade : Grade) -> Nat
countMatchingGrades students grade = List.length matches
  where
    matches : List StudentData
    matches = filter (gradeMatch grade) students

export
getMajors: List StudentData -> List String
getMajors students = sort $ nub $ concat $ (.majors) <$> students

export
matchMajor : String -> (student : StudentData) -> Bool
matchMajor major student = major `elem` student.majors


export 
engineer : (student : StudentData) -> Bool
engineer student = isInfixOf "Engineering" $ student.school

export 
artsSciences : (Student : StudentData) -> Bool
artsSciences student = isInfixOf "Arts & Sciences" $ student.school

export 
studentClass : (className : String) -> StudentData -> Bool
studentClass className student = className == student.level

export 
countPred : (pred : StudentData -> Bool) -> ( sl : List StudentData) -> Nat
countPred pred sl = length $ filter pred sl


summarizeGrades : (letterGrades : List Grade) 
                -> (students : List StudentData) 
                -> List ( Grade, Nat )
summarizeGrades letterGrades students = 
  summarizeGrade' <$> letterGrades
  where
    summarizeGrade' : Grade -> ( Grade, Nat )
    summarizeGrade' grade =
       (grade, countMatchingGrades students grade)
