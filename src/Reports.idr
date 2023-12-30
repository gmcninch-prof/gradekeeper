module Reports


import Control.Monad.Reader
import Control.Monad.Identity

import Data.Vect
import Data.List
import Data.String

import Md
import Student
import Course
import Results
import LetterGrades
import Stats
import Util
import State

import IdrisTime


summarizeGrades : (letterGrades : List Grade) -> (results : List StudentResult) -> List ( Grade, Nat )
summarizeGrades letterGrades results = summarizeGrade' <$> letterGrades
  where
    summarizeGrade' : Grade -> ( Grade, Nat )
    summarizeGrade' grade = 
      (grade, countGrades letterGrades grade (courseScore <$> results))


studentReport : ( student : StudentResult ) -> Reader ResultState MDPar
studentReport student = do
  state <- ask 
  let course : Course = state.course    
  rawFormulas <- the (List Formula) <$> getFormulas student.id

  pure $
    Section { header = [ Text student.name ]
            , contents = info
                         <+> [ outcomes course ] 
                         <+>  (markupFormula course <$> rawFormulas)
                         <+>  grades
                         <+> intersperse (Normal [ Text "" ] ) (topLink <$> student.section) 
                         <+> [ Normal [ Text "\\newpage" ]                       
                             , Normal [ Text "" ]
                             , Normal [ Text "-----" ]
                             ]
            , id = Just student.id
            }

  where
      topLink : String -> MDPar
      topLink sect = Normal [ Link $ MkHRef "#top-\{remSpaces sect}" "return to \{sect} summary" ]
    
      info : List MDPar
      info = [ ListItem { header = [ Text $ "id: \{student.id}"], contents = []}
             , ListItem { header = [ Text $ "level: " ++ student.level], contents = []}
             , ListItem { header = [ Text $ "majors: " ++ joinBy ", " student.majors], contents = []}
             , ListItem { header = [ Text $ "school: " ++ student.school], contents = []}           
             ]

      cscore : Course -> ScoreComponent -> Maybe Double
      cscore course comp = componentScore course student.outcomes comp
    
      markupFormula : Course -> Formula -> MDPar
      markupFormula course f = 
        let formulaBody : List (Vect 4 MDText)
            formulaBody = (\comp => [ Text $ comp.compName
                                    , Text $ show $ comp.weight
                                    , Text $ maybe "--" (show . round 2) $ cscore course comp 
                                    , Text $ maybe "--" (show . round 2 . (* comp.weight)) $ cscore course comp
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

      contents : Course -> List (Vect 3 MDText)
      contents course = (\out => Text <$> 
        [ out.label 
        , maybe "--" (show . round 2)  (getOutcomeByLabel course student.outcomes out.label)
        , maybe "--" (displayRawScore . .value) (getRawOutcomeByLabel student.outcomes out.label)
        ])
        <$> 
        student.outcomes
      
      outcomes : Course -> MDPar
      outcomes course = Table { header = Bold <$> [ "Item", "Score", "raw scores" ] 
                              , contents = contents course
                              }

      grades : List MDPar
      grades = [ ListItem [ Bold "Score: " , Text $ (show . round 2 ) student.courseScore ] []
               , ListItem [ Bold "Grade: " , Text student.grade ] []
               ]
  
           

statsReport : Reader ResultState MDPar
statsReport = do
  state <- ask
  let course = state.course
      students = state.studentResults
  
  pure $   
    Section { header = [ Text "Statistics" ] 
            , contents = [ ListItem { header = [ Text "Levels"], contents =[ levelTable students ] }
                         , ListItem { header = [ Text "School"], contents = [ schoolTable students ] }
                         , ListItem { header = [ Text "Majors"], contents = [ majorsTable students ] }
                         , ListItem { header = [ Text "Score statistics" ]
                                    , contents = [ classMedian students
                                                 , classMean students
                                                 ]
                                    }
                         , ListItem { header = [ Text "Grades"], contents = [ gradeTable students ] }
                         , Normal [ Text "\\newpage" ]                                      
                         , Normal [ Text "" ]                                
                         , Normal [ Text "------" ]  
                         ]
            , id = Just "statistics"
            }           
  where
    classMedian : List StudentResult -> MDPar
    classMedian students
      = ListItem { header = [ Text $ "median: " ++ show m ]
                 , contents = []
                 }
      where
        m : Maybe Double
        m = map (round 2) (median $ .courseScore <$> students)

    classMean : List StudentResult -> MDPar
    classMean students = 
      ListItem { header = [ Text $ "mean: " ++ show  m ]
               , contents = []
               }
      where
        m : Double
        m = round 2 $ mean $ .courseScore <$> students  
      
    levelTable : List StudentResult -> MDPar
    levelTable students = 
      Table { header = [ Text "Level", Text "#" ]
            , contents = (\class => Text <$> [ class, show $ (countPred (studentClass class) students)])
                          <$> [ "First Year", "Sophomore", "Junior", "Senior" ]
            }


    schoolTable : List StudentResult -> MDPar
    schoolTable students = 
      Table { header = [ Text "School", Text "#" ]
            , contents = [ [ Text "A&S", Text $ show $ countPred artsSciences students ]
                         , [ Text "SoE", Text $ show $ countPred engineer students ]
                         ]                                     
                       }

    majorsTable : List StudentResult -> MDPar
    majorsTable students = 
      Table { header = [ Text "Schools & Majors", Text "#" ]
            , contents = (\major => [ Text major, Text $ show $ countPred (matchMajor major) students ])<$>
                         getMajors students
                       }




    gradeTable : List StudentResult -> MDPar
    gradeTable students = 
      Table { header = [ Text "Grade", Text "#" ]
            , contents = (\grade => Text <$> [ grade, show $ (countPred (gradeMatch grade) students)])
                         <$> [ "A", "B", "C", "D", "F" ]
            }


public export 
mkCourseReport : Reader ResultState MD
mkCourseReport = do
  state <- ask
  let course = state.course
      students = state.studentResults
  
  stats <- statsReport 
  
  studentSections <- traverse studentReport students
  
  pure $ MkMD { date = Just $ state.date
              , title = Just $ course.title ++ " " ++ show course.semester
              , author = Nothing
              , content = [ stats ] <+> (mdSection course students <$> sections students)  <+> studentSections
              , fileName = reportFileName course
              }
  where
    headers : Vect 4 MDText
    headers = Bold <$> [  "Id", "Name", "Score", "Grade" ]
  
    sections : List StudentResult -> List String
    sections students = Data.List.sort $ Data.List.nub $ concat $ (.section <$> students)
  
    bySection : String -> List StudentResult -> List StudentResult
    bySection sec students = filter (\student => sec `elem` student.section) students
  
    initItems : (student : StudentResult) -> Vect 4 MDText
    initItems student = 
      [ Text student.id
      , Link $ MkHRef {target = "#" ++ student.id, desc = student.name }
      , Text $ (show . round 2) student.courseScore
      , Text student.grade
      ]
  
    contents : List StudentResult -> String -> List (Vect 4 MDText)
    contents students sect = initItems <$> (bySection sect students)
  
    table : List StudentResult -> String -> MDPar
    table students sect = Table { header = headers
                                , contents = contents students sect
                                } 

    mdSection : Course -> List StudentResult -> String -> MDPar
    mdSection course students sect =  
      Section { header = [Text course.title, Text sect,  Text (show course.semester)]
              , contents = [ table students sect
                           , Normal [ Text "\\newpage" ]                              
                           , Normal [ Text "" ]
                           , Normal [ Text "-----" ]
                           ]
              , id = Just $ "top-\{remSpaces sect}"
              }

