module Reports

import MD
import Student
import Course
import Data.Vect
import LetterGrades


infixr 5 ^

(^) : Double -> Nat -> Double
(^) dbl 0 = 1.0
(^) dbl (S k) = dbl * (dbl ^ k)

round' : Double -> Integer
round' dbl = if abs < 0.5 then floor else floor + 1
  where
    floor : Integer
    floor = cast dbl
    
    abs : Double
    abs = dbl - fromInteger floor

public export
round : (prec : Nat) -> (num : Double) -> Double
round prec num = cast (round' $ num * mod ) / mod
  where
    mod : Double
    mod = 10^prec

summarizeGrades : (letterGrades : List Grade) -> (results : List StudentResult) -> List ( Grade, Nat )
summarizeGrades letterGrades results = summarizeGrade' <$> letterGrades
  where
    summarizeGrade' : Grade -> ( Grade, Nat )
    summarizeGrade' grade = 
      (grade, countGrades letterGrades grade (courseScore <$> results))


public export 
mkCourseReport : (course : Course) -> (students : List StudentResult) -> MD
mkCourseReport course students = 
  MkMD { date = Nothing
       , title = Nothing
       , author = Nothing
       , content = [ section ]
       }
  where
    headers : Vect 4 MDText
    headers = Bold <$> [ "Name", "Id", "Score", "Grade" ]
  
    initItems : (student : StudentResult) -> Vect 4 TableContents
    initItems student = 
    CString <$> [ student.name
                , student.id
                , (show . round 2) student.courseScore
                , student.grade
                ]
    
  
    contents : List (Vect 4 TableContents)
    contents = initItems <$> students
  
    table : MDPar
    table = Table { header = headers
                  , contents = contents
                  }

    section : MDPar
    section =  Section { header = [Text course.title, Text (show course.semester)]
                       , contents = [ table
                                    , Normal [ Text "\\newpage" ]
                                    ]
                       }
