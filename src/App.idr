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
  (_,args) <- Data.List.splitAt 1 <$> System.getArgs
  
  traverse_ (\a => putStrLn $ "reading: " ++ a) args
  
  eReports <- traverse getReportByFile args
  traverse_ write eReports
  

