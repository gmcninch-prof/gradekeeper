module App

import System
import System.Console.GetOpt
import Process
import MD

import Data.List

help : IO ()
help = do
  putStrLn "Help!"
  
  

main : IO ()
main =  do
  (_,args) <- Data.List.splitAt 1 <$> System.getArgs
  putStrLn $ show args
  let result : Result ()
      result = getOpt RequireOrder [] args
  putStrLn $ show result.nonOptions
  eReports <- traverse getReportByFile result.nonOptions
  traverse_ write eReports
  

