module Student


--import Data.String
import JSON.Derive
import Data.SortedMap

import LetterGrades
import Course
import Util

%language ElabReflection

public export 
data ScoreT : Type where
  Score : ( score : Double ) -> ScoreT
  ListScores : (scores : List Double) -> ScoreT


untaggedOptions : Options
untaggedOptions = { sum := UntaggedValue
                  , constructorTagModifier := toLower } defaultOptions

  
singleOptions : Options
singleOptions = { sum := ObjectWithSingleField
                , constructorTagModifier := toLower } defaultOptions


public export 
displayRawScore : ScoreT -> String
displayRawScore (Score score) = show $ round 2 score
displayRawScore (ListScores scores) = show $ (round 2) <$> scores


%runElab derive "ScoreT" [Show, Eq, customToJSON untaggedOptions, customFromJSON untaggedOptions ]

public export 
record Outcome where
  constructor MkOutcome
  label  : String
  value  : ScoreT

%runElab derive "Outcome" [Show, Eq, ToJSON, FromJSON]


public export
record StudentDetails where
  constructor MkStudentDetails
  email : Maybe String
  majors : Maybe String
  level : Maybe String 
  school : Maybe String

%runElab derive "StudentDetails" [ Show, Eq, ToJSON, FromJSON ]

public export
record StudentData where
  constructor MkStudentData
  name    : String
  id      : String
  section : List String
  inSIS   : Bool  
  details : StudentDetails
  outcomes : List Outcome

%runElab derive "StudentData" [Show, Eq, ToJSON, FromJSON]

public export
record StudentResult where
  constructor MkStudentResult
  name    : String
  id      : String
  section : List String
  details : StudentDetails
  courseScore : Double
  grade       : String
  outcomes    : List Outcome

%runElab derive "StudentResult" [Show, Eq, ToJSON, FromJSON]


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
getRawOutcomeByLabel : (course : Course) -> (outs: List Outcome) -> (label : String) -> Maybe Outcome
getRawOutcomeByLabel course outs label =
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

    scores : {comps: List ScoreComponent} -> Maybe (Vect (length comps) Double)
    scores {comps} = do
      traverse (componentScore course outcomes) compsV
      where
        compsV : Vect (length comps) ScoreComponent
        compsV =fromList comps


    weights : {comps: List ScoreComponent} -> Vect (length comps) Double
    weights {comps} = getWt <$> fromList comps

export
result : (course:Course) -> (student:StudentData) -> Maybe StudentResult      
result course student = do
  results <- traverse (scoreForFormula course student.outcomes) course.formulas
  score <- round 2 <$> maxL results
  let lg = case course.grades of
                Nothing => letterGrades
                (Just x) => x

  let grade = computeGrade lg score
  pure $ MkStudentResult { name = student.name
                         , id = student.id
                         , section = student.section
                         , details = student.details
                         , courseScore = score
                         , grade = grade
                         , outcomes = student.outcomes
                         }








