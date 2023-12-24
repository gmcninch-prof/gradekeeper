module App

import System
import Process
import Md

import Data.List

help : IO ()
help = do
  putStrLn "Help!"
  
  

main : IO ()
main =  do
  allArgs <- System.getArgs
  let (_,args) =  Data.List.splitAt 1 allArgs

  putStrLn $ show args
      
  traverse_ (\a => putStrLn $ "reading: " ++ a) args
  
  eReports <- traverse getReportByFile args
  traverse_ write eReports
  

