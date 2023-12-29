module App

import Control.Monad.Reader
import Control.Monad.Identity
import Data.List
import JSON.Derive
import System
import System.File.ReadWrite

import Course
import IdrisTime
import Student
import Md
import Reports 
import Results
import State
import Util

-- computeResults : List StudentData -> Reader State (List StudentResult)
-- computeResults sd = do
--   state <- ask
--   let results : List StudentResult = mapMaybe (result state.course state.exceptions) sd
--   pure results



courseReport : Reader State MD
courseReport = do
  state <- ask
  let resultstate : ResultState = runReader state getResultState 
  let report : MD = runReader resultstate mkCourseReport 
  pure report 
    

runReports : String -> IO (Either String MD)
runReports filename = do
  stateE <- getState filename
  case stateE of
       (Left err) => pure $ Left err
       (Right state) => pure $ Right $ runReader state courseReport 


public export
write : Either String MD -> IO ()
write (Left err) = putStrLn $ "error: " ++ err
write (Right md) = do
  ignore $ writeMD md


main : IO ()
main =  do
  allArgs <- System.getArgs
  let (_,args) =  Data.List.splitAt 1 allArgs

  putStrLn $ show args
      
  traverse_ (\a => putStrLn $ "reading: " ++ a) args
  
  eReports <- traverse runReports args
  traverse_ write eReports
  

