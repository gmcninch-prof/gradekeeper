module Reports

import Md
import Student
import Course
import Data.Vect
import Data.List
import Data.String
import LetterGrades
import Stats

import IdrisTime

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

remSpaces : String -> String
remSpaces str = 
  concat $ words str

summarizeGrades : (letterGrades : List Grade) -> (results : List StudentResult) -> List ( Grade, Nat )
summarizeGrades letterGrades results = summarizeGrade' <$> letterGrades
  where
    summarizeGrade' : Grade -> ( Grade, Nat )
    summarizeGrade' grade = 
      (grade, countGrades letterGrades grade (courseScore <$> results))


studentReport : ( course: Course) -> ( student : StudentResult) -> MDPar
studentReport course student = 
  Section { header = [ Text student.name ]
          , contents = info
                       <+> [ outcomes ] 
                       <+>  formulas 
                       <+>  grades
                       <+> (topLink <$> student.section)
                       <+> [ Normal [ Text "" ]
                           , Normal [ Text "-----" ]
                           ]
          , id = Just student.id
          }

  where 
    topLink : String -> MDPar
    topLink sect = Normal [ Link $ MkHRef "#top-\{remSpaces sect}" "return to \{sect} summary" ]
  
    info : List MDPar
    info = [ ListItem { header = [ Text $ "level: " ++ fromMaybe "" student.details.level], contents = []}
           , ListItem { header = [ Text $ "majors: " ++ fromMaybe "" student.details.majors], contents = []}
           , ListItem { header = [ Text $ "school: " ++ fromMaybe "" student.details.school], contents = []}           
           ]
  
    contents : List (Vect 2 MDText)
    contents = (\out => Text <$> 
      [ out.label 
      , maybe "--" (show . round 2) $ getOutcomeByLabel course student.outcomes out.label ] ) 
      <$> 
      student.outcomes
    
    outcomes : MDPar
    outcomes = Table { header = Bold <$> [ "Item", "Score" ] 
                     , contents = contents 
                     }
  
    cscore : ScoreComponent -> Maybe Double
    cscore comp = componentScore course student.outcomes comp
  
    formula : Formula -> MDPar
    formula f = 
      let formulaBody : List (Vect 4 MDText)
          formulaBody = (\comp => [ Text $ comp.compName
                                  , Text $ show $ comp.weight
                                  , Text $ maybe "--" (show . round 2) $ cscore comp 
                                  , Text $ maybe "--" (show . round 2 . (* comp.weight)) $ cscore comp
                                  ]) 
                                  <$> f.formula 
                                  
          formulaSummary : Vect 4 MDText
          formulaSummary = [ Text "", Text "", Text ""
                           , Text $ maybe "--" (show . round 2) $ scoreForFormula course student.outcomes f
                           ]
      in
      Section { header = [ Text f.id ]
              , contents = [ Table { header = Bold <$> [ "component", "weight", "score", "" ] 
                                                       , contents = formulaBody <+> [ formulaSummary ]
                                   }
                           ]
              , id = Nothing
              }

    formulas  : List MDPar
    formulas = formula <$> course.formulas 
   
    grades : List MDPar
    grades = [ ListItem [ Bold "Score: " , Text $ (show . round 2 ) student.courseScore ] []
             , ListItem [ Bold "Grade: " , Text student.grade ] []
             ]

statsReport : (course: Course) -> (students : List StudentResult) -> MDPar            
statsReport course students = 
  Section { header = [ Text "Statistics" ] 
          , contents = [ ListItem { header = [ Text "Levels"], contents =[ levelTable ] }
                       , ListItem { header = [ Text "School"], contents = [ schoolTable ] }
                       , ListItem { header = [ Text "Score statistics" ]
                                  , contents = [ classMedian
                                               , classMean
                                               ]
                                  }
                       , ListItem { header = [ Text "Grades"], contents = [ gradeTable ] }
                       , Normal [ Text "" ] 
                       , Normal [ Text "------" ]  
                       ]
          , id = Just "statistics"
          }           
  where
    classMedian : MDPar
    classMedian = ListItem { header = [ Text $ "median: " ++ show m ]
                           , contents = []
                           }
      where
        m : Maybe Double
        m = map (round 2) (median $ .courseScore <$> students)

    classMean : MDPar
    classMean = ListItem { header = [ Text $ "mean: " ++ show  m ]
                           , contents = []
                           }
      where
        m : Double
        m = round 2 $ mean $ .courseScore <$> students  
      
    levelTable : MDPar
    levelTable = Table { header = [ Text "Level", Text "#" ]
                       , contents = (\class => Text <$> [ class, show $ (countPred (studentClass class) students)])
                                    <$> [ "First Year", "Sophomore", "Junior", "Senior" ]
                       }


    schoolTable : MDPar
    schoolTable = Table { header = [ Text "Schools & Majors", Text "#" ]
                        , contents = [ [ Text "A&S", Text $ show $ countPred artsSciences students ]
                                     , [ Text "SoE", Text $ show $ countPred engineer students ]
                                     , [ Text "Math Majors/minors", Text $ show $ countPred mathMajor students ]
                                     ]
                       }


    gradeTable : MDPar
    gradeTable = Table { header = [ Text "Grade", Text "#" ]
                       , contents = (\grade => Text <$> [ grade, show $ (countPred (gradeMatch grade) students)])
                                    <$> [ "A", "B", "C", "D", "F" ]
                       }


public export 
mkCourseReport : (course : Course) -> (students : List StudentResult) -> (date : String) -> MD
mkCourseReport course students date = 
  MkMD { date = Just $ date
       , title = Just $ course.title ++ " " ++ show course.semester
       , author = Nothing
       , content = (mdSection <$> sections) ++ [ statsReport course students] <+> studentSections
       , fileName = reportFileName course
       }
  where
    headers : Vect 4 MDText
    headers = Bold <$> [  "Id", "Name", "Score", "Grade" ]
  
    sections : List String
    sections = Data.List.sort $ Data.List.nub $ concat $ (.section <$> students)
  
    bySection : String -> List StudentResult -> List StudentResult
    bySection sec students = filter (\student => sec `elem` student.section) students
  
    initItems : (student : StudentResult) -> Vect 4 MDText
    initItems student = 
      [ Text student.id
      , Link $ MkHRef {target = "#" ++ student.id, desc = student.name }
      , Text $ (show . round 2) student.courseScore
      , Text student.grade
      ]
  
    contents : String -> List (Vect 4 MDText)
    contents sect = initItems <$> (bySection sect students)
  
    table : String -> MDPar
    table sect = Table { header = headers
                       , contents = contents sect
                       }

    mdSection : String -> MDPar
    mdSection sect =  Section { header = [Text course.title, Text sect,  Text (show course.semester)]
                              , contents = [ table sect
                                           , Normal [ Text "" ]
                                           , Normal [ Text "-----" ]
                                           ]
                              , id = Just $ "top-\{remSpaces sect}"
                              }

    studentSections : List MDPar
    studentSections = studentReport course <$>  students
