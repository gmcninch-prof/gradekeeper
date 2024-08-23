module Results

import Control.Monad.Reader
import Control.Monad.Identity
import Data.SortedMap
import Data.List
import Data.Vect
import Data.SortedMap

import LetterGrades
import Util
import CourseData
import State



average : List Double -> Double
average dbls = (1/len)*sum dbls
  where
    len : Double
    len = cast $ length dbls

export
dropAndAverage : (num:Nat) -> List Double -> Double
dropAndAverage num dbls = average pruned
  where
    sorted : List Double
    sorted = sort dbls
  
    pruned : List Double
    pruned = let (_,p) = splitAt num sorted in p
    

getDrops : (course : Course) -> (scoreName : String) -> Nat
getDrops course scoreName = 
  case lookup scoreName scoreMap of
       Nothing => 0
       (Just x) => case x.drops of
                        Nothing => 0
                        (Just y) => y
  where
    scoreMap : SortedMap String Score
    scoreMap = fromList $ (\score => (score.scoreName,score)) <$> course.scores

outcomeScore : (course : Course) -> (outcome:Outcome) -> Double
outcomeScore course outcome = dropAndAverage drops outcome.marks
  where
    drops : Nat
    drops = getDrops course outcome.scoreName
    
export
maxL : Ord a => List a -> Maybe a
maxL [] = Nothing
maxL (x :: xs) = do
  case maxL xs of
       Nothing => pure x
       (Just y) => pure $ max x y

export
minL : Ord a => List a -> Maybe a
minL [] = Nothing
minL (x :: xs) = do
  case minL xs of
       Nothing => pure x
       (Just y) => pure $ min x y

formulaComponentScore : (course : Course) -> (outcomes : List Outcome) -> FormulaComponent -> Maybe Double
formulaComponentScore course outcomes fc = 
  case fc.component of
       (Value scoreName) => do
         outcome <- lookup scoreName outcomeMap
         pure $ outcomeScore course outcome 
       (Max sn1 sn2) => do
         s1 <- outcomeScore course <$> lookup sn1 outcomeMap
         s2 <- outcomeScore course <$> lookup sn2 outcomeMap
         pure $ max s1 s2 
       (Min sn1 sn2) => do
         s1 <- outcomeScore course <$> lookup sn1 outcomeMap
         s2 <- outcomeScore course <$> lookup sn2 outcomeMap
         pure $ min s1 s2 
       (Maxl snl) => do
         outcomes <- traverse (\sn => lookup sn outcomeMap) snl
         let sl = outcomeScore course <$> outcomes
         maxL sl
       (Minl snl) => do
         outcomes <- traverse (\sn => lookup sn outcomeMap) snl
         let sl = outcomeScore course <$> outcomes
         minL sl
  where
    outcomeMap : SortedMap String Outcome
    outcomeMap = fromList $ (\outcome => (outcome.scoreName,outcome)) <$> outcomes


export
dotProduct : Vect n Double -> Vect n Double -> Double
dotProduct xs ys = sum $ zipWith (*) xs ys

export
scoreForFormula : (course : Course) -> (outcomes : List Outcome) -> Formula -> Maybe Double
scoreForFormula course outcomes formula =  do
  let comps = formula.formulaComponents
      
      fcs : Vect (length comps) FormulaComponent
      fcs = Data.Vect.fromList comps
      
      w : Vect (length comps) Double
      w = (\fc => fc.weight) <$> fcs
      
  scores <- traverse (formulaComponentScore course outcomes) fcs
      
  pure $ dotProduct w scores

export
getFormulasForStudent : (studentId : String) -> Reader State (List Formula)
getFormulasForStudent studentId =  do
  state <- ask
  let course: Course = state.course

      fmap : SortedMap String Formula
      fmap = fromList $ (\f => (f.id,f)) <$> course.formulas ++ course.exceptFormulas

      getFormula : String -> Maybe Formula
      getFormula id = lookup id fmap
        
      emap : SortedMap String StudentException
      emap = fromList $ (\e => (e.id,e)) <$> course.exceptions  
        
  case lookup studentId emap of
    Nothing => pure $ course.formulas
    Just se => pure $ mapMaybe getFormula se.formulas  

isIncomplete : (studentId : String) -> Reader State Bool
isIncomplete studentId = do
  state <- ask
  let course = state.course
      exceptions = course.exceptions
  
      emap : SortedMap String StudentException
      emap = fromList $ (\e => (e.id,e)) <$> exceptions
  case lookup studentId emap of
       Nothing => pure False
       Just se => pure se.incomplete
  

export
result : (student:StudentData) -> Reader State StudentData
result student = do
  state <- ask
  
  formulas <- getFormulasForStudent student.id
  
  incomplete <- isIncomplete student.id  

  let course : Course = State.State.(.course) state
      results : Maybe (List Double) =  traverse (scoreForFormula course student.outcomes) formulas
      
      score : Double
      score = case round 2 <$> (results >>= maxL) of
                   Nothing => 0
                   Just sc => sc
                   
      lg = case course.grades of
                Nothing => defaultLetterGrades
                (Just x) => x

      grade : String
      grade  = if incomplete then "Incomplete" else computeGrade lg score
  pure $ { courseScore := Just score
         , grade := Just grade
         } student
  
    


public export
getResultState : Reader State State
getResultState = do
  state <- ask
  studentResults <- traverse result state.studentdata
  pure $ { studentdata :=studentResults } state
