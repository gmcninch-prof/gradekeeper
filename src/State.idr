module State 

import Control.Monad.Reader
import JSON.Derive

import Student
import Course
import Util
import IdrisTime

%language ElabReflection

public export
record State where
  constructor MkState
  date : String
  course : Course
  exceptions : List StudentException
  studentdata : List StudentData

%runElab derive "State" [Show, Eq]  

public export
record ResultState where
  constructor MkResultState
  date: String
  course: Course
  exceptions: List StudentException
  studentResults: List StudentResult


public export
interface IsCourse a where
  cdate : a -> String
  ccourse : a -> Course
  cexceptions : a -> List StudentException
  
public export  
IsCourse State where
  cdate = .date
  ccourse = .course
  cexceptions = .exceptions

public export
IsCourse ResultState where
  cdate = .date
  ccourse = .course
  cexceptions = .exceptions


assembleState : String -> Course -> List StudentException -> List StudentData -> Either String State
assembleState date course excepts studentdata = 
  pure $ MkState date course excepts studentdata


eraseEitherList : Either a (List b) -> List b
eraseEitherList (Left _) = []
eraseEitherList (Right xl) = xl

public export 
getState : (courseFileName : String) -> IO (Either String State)
getState courseFileName =   do
  t <- getTime
  date <- strftime "%Y-%m-%d %H:%M:%S %Z" t
  ce <- decodefile courseFileName

  case ce of
    Left err => pure $ Left err
    Right course => do
      sle <- decodefile $ String.joinBy "/" [ course.dataDir, course.courseJSONFile ]
      excepts <- case course.exceptionsFile of
                      Nothing => pure []
                      Just f => eraseEitherList <$> (decodefile $ String.joinBy "/" [ course.dataDir, f ])
      pure $  MkState date course excepts <$> sle

