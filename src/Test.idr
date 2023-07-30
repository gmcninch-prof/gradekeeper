module Test

-- import Course
-- import Student
-- import LetterGrades 
-- import Data.Nat


p1: Nat -> Type
p1 n = (n=2)

testReplace: (x=y) -> (p1 x) -> (p1 y)
testReplace a b = ?replace ?a ?b


