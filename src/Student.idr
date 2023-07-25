module Student


--import Data.String
import JSON.Derive
import Data.SortedMap

import LetterGrades
import Course

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


%runElab derive "ScoreT" [Show, Eq, customToJSON untaggedOptions, customFromJSON untaggedOptions ]

public export 
record Outcome where
  constructor MkOutcome
  label  : String
  value  : ScoreT

%runElab derive "Outcome" [Show, Eq, ToJSON, FromJSON]


public export
record StudentSISData where
  constructor MkStudentSISData
  name    : String
  section : String
  outcomes : List Outcome

%runElab derive "StudentSISData" [Show, Eq, ToJSON, FromJSON]

export
record StudentResult where
  constructor MkStudentResult
  name    : String
  section : String
  courseScore : Double
  grade       : String


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
    
  
outcomeScore : (course : Course) -> (outcome:Outcome) -> (avg:List Double -> Double) -> (student:StudentSISData) -> Double
outcomeScore course outcome avg student = case outcome.value of
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

export
getOutcomeByLabel : (course: Course) -> (student:StudentSISData) -> (label : String) -> Maybe Double
getOutcomeByLabel course student label = do 
  outcome <- lookup label studentOutcomes
  case lookup label courseStrategies of
       Nothing => 
         Just $ outcomeScore course outcome average student
       (Just (MkComputeStrategy str Average)) => 
         Just $ outcomeScore course outcome average student
       (Just (MkComputeStrategy str (DropAndAverage num))) => 
         Just $ outcomeScore course outcome (dropAndAverage num) student
  where
    studentOutcomes : SortedMap String Outcome
    studentOutcomes = fromList $ map (\res => (res.label,res)) student.outcomes
    
    courseStrategies : SortedMap String ComputeStrategy
    courseStrategies = fromList $ map (\strat => (strat.label,strat)) course.strategies

componentScore : (course : Course) -> (student : StudentSISData) -> (component : ScoreComponent)  -> Maybe Double
componentScore course student  (Copy compName label weight) = 
  getOutcomeByLabel course student label 
componentScore course student (Max compName labels weight) =  do
  r <- ress
  maxL r
  where
    ress : Maybe (List Double)
    ress = traverse (getOutcomeByLabel course student) labels
componentScore course student (Min compName labels weight) = do
  r <- ress
  minL r
  where
    ress : Maybe (List Double)
    ress = traverse (getOutcomeByLabel course student) labels

export
dotProduct : Vect n Double -> Vect n Double -> Double
dotProduct xs ys = sum $ zipWith (*) xs ys


export
scoreForFormula : (course : Course) -> (student : StudentSISData) -> Formula -> Maybe Double
scoreForFormula course student (MkFormula id comps) = do
  ss <- scores {comps}
  pure $ dotProduct weights ss
  where
    getWt : ScoreComponent -> Double
    getWt (Copy _ _ weight) = weight
    getWt (Max _ _ weight) = weight
    getWt (Min _ _ weight) = weight

    scores : {comps: List ScoreComponent} -> Maybe (Vect (length comps) Double)
    scores {comps} = do
      traverse (componentScore course student) compsV
      where
        compsV : Vect (length comps) ScoreComponent
        compsV =fromList comps


    weights : {comps: List ScoreComponent} -> Vect (length comps) Double
    weights {comps} = getWt <$> fromList comps

export
result : (course:Course) -> (student:StudentSISData) -> Maybe StudentResult      
result course student = do
  results <- traverse (scoreForFormula course student) course.formulas
  score <- maxL results
  let lg = case course.grades of
                Nothing => letterGrades
                (Just x) => x

  let grade = computeGrade lg score
  pure $ MkStudentResult { name = student.name
                         , section = student.section
                         , courseScore = score
                         , grade = grade
                         }

export
report : (course:Course) -> (student:StudentSISData) -> String
report course student = 
  case r of
       Nothing => a ++ " -- error"
       (Just x) => a ++ ", " ++ (joinBy ", "  (show <$> x))
  where
    rr : Maybe StudentResult
    rr = result course student
  
    a : String
    a = case rr of
             Nothing => student.name ++ " - no reported scored: "
             (Just x) => joinBy ", " [ x.name
                                      , x.grade
                                      , show x.courseScore
                                      ]
  
    r : Maybe (List Double)
    r = traverse (getOutcomeByLabel course student) (label <$> student.outcomes)



summarizeGrades : (letterGrades : List Grade) -> (results : List StudentResult) -> List ( Grade, Nat )
summarizeGrades letterGrades results = summarizeGrade' <$> letterGrades
  where
    summarizeGrade' : Grade -> ( Grade, Nat )
    summarizeGrade' grade = 
      (grade, countGrades letterGrades grade (courseScore <$> results))






