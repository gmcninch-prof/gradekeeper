module Stats

import Student
import Data.Maybe
import Data.SortedMap
import Data.String 
import Data.Nat
import Data.Vect
import Data.Vect.Sort
import Data.List1
import Control.Ord

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


-- export
-- median : List Double -> Maybe Double
-- median items with (length (sort items))
--   median items | 0 = Nothing
--   median items | (S k) = do
--     let sitems = sort items
--     idx <- natToFin (div k 2) (length sitems)
--     pure $ index' sitems idx


data Result a = Even a a
              | Odd a

middle : { a: Type} -> Vect (S n) a -> Result a
middle (x :: []) = Odd x
middle (x :: (y :: [])) = Even x y
middle (x :: (y :: (z :: xs))) = middle $ init (y :: (z :: xs))


medianV : { a: Type} -> Ord a => Num a => Fractional a => {n:Nat} -> Vect (S n) a -> a
medianV {n} xs = case middle $ Data.Vect.Sort.sort xs of
                      (Even x y) => (x+y)/2
                      (Odd x) => x 


median1 : {a:Type} -> Ord a => Num a => Fractional a => List1 a -> a
median1 {a} xs = 
  case xs of
    (head ::: tail) => medianV $ fromList $ (head :: tail)

export
median : List Double -> Double
median [] = 0
median (x :: xs) = median1 (x ::: xs)

