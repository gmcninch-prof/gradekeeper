module Stats

import Student
import Data.Maybe
import Data.SortedMap
import Data.String 
import Data.Nat
import Data.Vect

export 
gradeMatch : (grade : String) -> ( student : StudentResult) -> Bool
gradeMatch grade student = 
  case lookup grade gradeMap of
    Nothing => False
    (Just gg) => elem student.grade gg
  where
    gradeMap : SortedMap String (List String)
    gradeMap = fromList [ ("A", [ "A+", "A", "A-" ] )
                        , ("B", [ "B+", "B", "B-" ] )
                        , ("C", [ "C+", "C", "C-" ] )
                        , ("D", [ "D+", "D", "D-" ] )
                        , ("F", [ "F" ])
                        ]

export
getMajors: List StudentResult -> List String
getMajors students = sort $ nub $ concat $ (.majors) <$> students

export
matchMajor : String -> (student : StudentResult) -> Bool
matchMajor major student = major `elem` student.majors


export 
engineer : (student : StudentResult) -> Bool
engineer student = isInfixOf "Engineering" $ student.school

export 
artsSciences : (Student : StudentResult) -> Bool
artsSciences student = isInfixOf "Arts & Sciences" $ student.school



export 
studentClass : (className : String) -> StudentResult -> Bool
studentClass className student = className == student.level

export 
countPred : (pred : StudentResult -> Bool) -> ( sl : List StudentResult) -> Nat
countPred pred sl = length $ filter pred sl

export
mean : List Double -> Double
mean scores = sum scores / cast (length scores)


export
median : List Double -> Maybe Double
median items with (length (sort items))
  median items | 0 = Nothing
  median items | (S k) = do
    let sitems = sort items
    idx <- natToFin (div k 2) (length sitems)
    pure $ index' sitems idx


