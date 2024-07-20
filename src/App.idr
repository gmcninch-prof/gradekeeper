module App

import Control.Monad.Reader
import Control.Monad.Identity
import Data.List
import JSON.Derive
import System
import System.File.ReadWrite
import System.Console.GetOpt

import Course
import IdrisTime
import Student
import Md
import Reports 
import Results
import State
import Util

import Data.List

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
    

runReports : String -> String -> IO (Either String MD)
runReports courseFile dataFile = do
  stateE <- getState courseFile dataFile
  case stateE of
       (Left err) => pure $ Left err
       (Right state) => pure $ Right $ runReader state courseReport 
       
         -- case state.course.status of
         --   Finished => pure $ Left $ "course \{show state.course.semester} \{state.course.title} completed"
         --   Current => pure $ Right $ runReader state courseReport 


public export
write : Either String MD -> IO ()
write (Left err) = putStrLn $ err
write (Right md) = do
  ignore $ writeMD md


 
optionSpec : OptDescr String
optionSpec = MkOpt { shortNames = ['c']
                   , longNames =  ["spec"]
                   , argDescr = (ReqArg id "CourseSpec") 
                   , description = "filename for course specs"
                   }

optionData : OptDescr String
optionData = MkOpt { shortNames = ['d']
                   , longNames = ["data"]
                   , argDescr =  (ReqArg id "CourseData") 
                   , description = "filename for course data"
                   }
 
 
 
main : IO ()
main =  do
  allArgs <- System.getArgs

  let result : GetOpt.Result String 
      result = getOpt Permute [ optionSpec , optionData] (drop 1 allArgs)


  case options result of
       [spec, dat] => do
         putStrLn "spec: \{spec}, dat: \{dat}"
         eReports <- runReports spec dat
         write eReports
       _ => putStrLn "Error"
    
