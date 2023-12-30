module Results

import Control.Monad.Reader
import Control.Monad.Identity
import Data.SortedMap
import Data.List
import Data.Vect


import LetterGrades
import Util
import Course
import State
import Student

-- ---------------------------------------------------------------------------------


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
    
  
outcomeScore : (course : Course) -> (outcome:Outcome) -> (avg:List Double -> Double) -> Double
outcomeScore course outcome avg = case outcome.value of
                                       (Score score) => score
                                       (ListScores scores) => avg scores

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

public export
getRawOutcomeByLabel : (outs: List Outcome) -> (label : String) -> Maybe Outcome
getRawOutcomeByLabel outs label =
  lookup label studentOutcomes
  where
    studentOutcomes : SortedMap String Outcome
    studentOutcomes = fromList $ map (\res => (res.label,res)) outs


public export
getOutcomeByLabel : (course: Course) -> (outs : List Outcome) -> (label : String) -> Maybe Double
getOutcomeByLabel course outs label = do 
  outcome <- lookup label studentOutcomes
  case lookup label courseStrategies of
       Nothing => 
         Just $ outcomeScore course outcome average
       (Just (MkComputeStrategy str Average)) => 
         Just $ outcomeScore course outcome average
       (Just (MkComputeStrategy str (DropAndAverage num))) => 
         Just $ outcomeScore course outcome (dropAndAverage num)
  where
    studentOutcomes : SortedMap String Outcome
    studentOutcomes = fromList $ map (\res => (res.label,res)) outs
    
    courseStrategies : SortedMap String ComputeStrategy
    courseStrategies = fromList $ map (\strat => (strat.label,strat)) course.strategies


export
componentScore : (course : Course) -> (outcomes : List Outcome) -> (component : ScoreComponent)  -> Maybe Double
componentScore course outcomes (MkScoreComponent compName computation weight) = 
  case computation of
       (Copy label) => getOutcomeByLabel course outcomes label 
       (Max labels) => do
         r <- traverse (getOutcomeByLabel course outcomes) labels
         maxL r
       (Min labels) => do
         r <- traverse (getOutcomeByLabel course outcomes) labels
         minL r


export
dotProduct : Vect n Double -> Vect n Double -> Double
dotProduct xs ys = sum $ zipWith (*) xs ys


export
scoreForFormula : (course : Course) -> (outcomes : List Outcome) -> Formula -> Maybe Double
scoreForFormula course outcomes (MkFormula id comps) = do
  ss <- scores {comps}
  pure $ dotProduct weights ss 
  where
    getWt : ScoreComponent -> Double
    getWt comp = comp.weight

    scores : (comps: List ScoreComponent) -> Maybe (Vect (length comps) Double)
    scores comps = do
      traverse (componentScore course outcomes) compsV
      where
        compsV : Vect (length comps) ScoreComponent
        compsV =fromList comps

    weights : {comps: List ScoreComponent} -> Vect (length comps) Double
    weights {comps} = getWt <$> fromList comps


export
getFormulas : IsCourse st => (studentId : String) -> Reader st (List Formula)
getFormulas studentId =  do
  s <- ask
  let course: Course = ccourse s
      exceptions: List StudentException = cexceptions s

      fmap : SortedMap String Formula
      fmap = fromList $ (\f => (f.id,f)) <$> course.formulas

      getFormula : String -> Maybe Formula
      getFormula id = lookup id fmap
        
      emap : SortedMap String StudentException
      emap = fromList $ (\e => (e.id,e)) <$> exceptions
  case lookup studentId emap of
    Nothing => pure $ mapMaybe getFormula course.gradingFormulas
    Just se => pure $ mapMaybe getFormula se.formulas  

isIncomplete : IsCourse st => (studentId : String) -> Reader st Bool
isIncomplete studentId = do
  s <- ask
  let exceptions : List StudentException = cexceptions s
  
      emap : SortedMap String StudentException
      emap = fromList $ (\e => (e.id,e)) <$> exceptions
  case lookup studentId emap of
       Nothing => pure False
       Just se => pure se.incomplete
  

export
result : (student:StudentData) -> Reader State StudentResult
result student = do
  state <- ask
  
  formulas <- getFormulas student.id
  
  incomplete <- isIncomplete student.id  

  let course : Course = State.State.(.course) state
      results : Maybe (List Double) =  traverse (scoreForFormula course student.outcomes) formulas
      score = case round 2 <$> (results >>= maxL) of
                   Nothing => 0
                   Just sc => sc
                   
      lg = case course.grades of
                Nothing => letterGrades
                (Just x) => x

      grade  = if incomplete then show Incomplete else computeGrade lg score
  pure $ MkStudentResult { name = student.name
                         , id = student.id
                         , section = student.section
                         , email = student.email
                         , level = student.level
                         , school = student.school
                         , majors = student.majors
                         , courseScore = score
                         , grade = grade
                         , outcomes = student.outcomes
                         }
  
    


public export
getResultState : Reader State ResultState
getResultState = do
  state <- ask
  studentResults <- traverse result state.studentdata
  pure $ MkResultState state.date state.course state.exceptions studentResults
