module Data 

import Data.List1
import Data.List
import Data.SortedMap
import Data.String
import Data.Maybe

import Student

%language ElabReflection

record EnrollmentData where
  constructor MkEnrollmentData
  name: String
  id: String
  email: String
  level: String
  school: String
  majors: List1 String

%runElab derive "EnrollmentData" [Show, Eq, ToJSON, FromJSON]

record OutcomeSpec where
  constructor MkOutcomeSpec
  label: String
  heading: String
  max: Integer

%runElab derive "OutcomeSpec" [Show, Eq, ToJSON, FromJSON]

record CSVOutcome where
  constructor MkOutcome
  spec : OutcomeSpec
  result : Double

record CanvasData where
  constructor MkCanvasData
  integrationID : String
  section : List String
  outcomes: List CSVOutcome
  



getCanvasData : List OutcomeSpec -> CSVRecord -> Maybe CanvasData
getCanvasData specs crec = do
  integrationID <- lookup "Integration ID" crec
  lsection <-lookup "Section" crec
  pure $ MkCanvasData { integrationID
                      , section = splitAnd lsection
                      , outcomes = Data.List.catMaybes $ getResult <$> specs
                      }

  where
    splitAnd : String -> List String
    splitAnd str = 
      let lol : List1 (List String)
          lol = Data.List.splitOn "and" $ words str
       in
      toList $ (Data.String.joinBy " ") <$> lol
  
    getResult : OutcomeSpec -> Maybe CSVOutcome
    getResult spec = do
      sresult <- lookup spec.label crec
      pure $ MkOutcome { spec, result = cast sresult }
    
  
enrollmentMap : List EnrollmentData -> SortedMap String EnrollmentData
enrollmentMap xs = ?enrollmentMap_rhs

canvasMap : List CanvasData -> SortedMap String CanvasData
canvasMap xs = ?canvasMap_rhs

getStudentData : List OutcomeSpec
               -> SortedMap String EnrollmentData
               -> SortedMap String CanvasData
               -> (id:String)
               -> Maybe StudentData
getStudentData spec ed cd id = do
  edata <- lookup id ed
  cdata <- lookup id cd
  pure $ MkStudentData 
   { name = edata.name
   , id
   , email = edata.email
   , level = edata.level
   , school = edata.school
   , majors = toList edata.majors
   , section = cdata.section
   , outcomes = getOutcomes cdata
   }
  where
    getOutcomes : CanvasData -> List Outcome
    getOutcomes x = ?getOutcomes_rhs
