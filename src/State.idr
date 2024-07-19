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
getState : (specFile : String) -> (dataFile:String) -> IO (Either String State)
getState specFile dataFile =   do
  t <- getTime
  date <- strftime "%Y-%m-%d %H:%M:%S %Z" t
  course <- decodefile specFile
  cdata <- decodefile dataFile

  case (course,cdata) of
    (Left err,_) => pure $ Left err
    (_,Left err) => pure $ Left err
    (Right xcourse,Right xcdata) => do
      excepts <- case xcourse.exceptionsFile of
                      Nothing => pure []
                      Just f => eraseEitherList <$> (decodefile f)
      pure $  Right $ MkState date xcourse excepts xcdata

