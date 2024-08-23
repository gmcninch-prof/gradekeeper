module State 

import Control.Monad.Reader
import JSON.Simple.Derive

import CourseData
import Util
import IdrisTime

%language ElabReflection

public export
record State where
  constructor MkState
  date : String
  course : Course
  studentdata : List StudentData

%runElab derive "State" [Show, Eq]  

-- eraseEitherList : Either a (List b) -> List b
-- eraseEitherList (Left _) = []
-- eraseEitherList (Right xl) = xl

public export 
getState : (specFile : String) -> (dataFile:String) -> IO (Either String State)
getState specFile dataFile =   do
  t <- getTime
  date <- strftime "%Y-%m-%d %H:%M:%S %Z" t
  course <- decodefile specFile
  studentData <- decodefile dataFile

  pure $ assembleState date course studentData

  where
    assembleState : String 
                  -> Either String Course 
                  -> Either String (List StudentData)
                  -> Either String State
    assembleState date ecourse edata = do
      course <- ecourse
      studentData <- edata
      pure $ MkState date course studentData
