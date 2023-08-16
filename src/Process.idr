module Process

import System.File.ReadWrite
import JSON.Derive

import Course
import Student
import Md
import Reports 

%language ElabReflection

decodefile : FromJSON a => String -> IO (Either String a)
decodefile filename = do 
  result <- readFile filename
  case result of
       (Left x)  => pure $ Left $ show x
       (Right x) => case decode  x of
                         (Left err) => pure $ Left $ show err
                         (Right y) => pure $ Right y  



dir : String
dir = "/home/george/Prof-Teach/scores/"

courseFiles : List String
courseFiles = [ String.joinBy "/" [ dir, "AY2021-2022--2022-sp--Math051.json" ]
              , String.joinBy "/" [ dir, "AY2021-2022--2022-sp--Math135.json" ]
              , String.joinBy "/" [ dir, "AY2022-2023--2023-sp--Math135.json" ]
              ]


explain : { a: Type} -> (msg : String) -> Maybe a -> Either String a
explain msg Nothing = Left msg
explain msg (Just x) = Right x


computeResults : Course -> List StudentData -> Either String (List StudentResult)
computeResults course students = do
  explain errmsg $ traverse (result course) students
  
  where
    errmsg : String
    errmsg = "Failed to construct student results"



econs : Either String a -> Either String (List a) -> Either String (List a)
econs x ll =  do
  xx <- x
  lll <- ll
  pure $ xx :: lll

getCourses : List String -> IO (Either String (List Course))
getCourses [] = pure $ Right []
getCourses (x :: xs) = do
  f <- the (Either String Course) <$> decodefile x
  rest <- getCourses xs
  pure $ econs f rest
    

getReportForCourse : Course -> List StudentData -> Either String MD
getReportForCourse course students = do
  results <- computeResults course students
  pure $ mkCourseReport course results


public export
getReportByFile : (courseFileName : String) -> IO (Either String MD)
getReportByFile courseFileName = do
  ce <- decodefile courseFileName
  putStrLn $ "working on " ++ courseFileName
  case ce of
    Left err => pure $ Left err
    Right course => do
      sle <- decodefile $ String.joinBy "/" [ course.dataDir, course.CanvasJSONFile ]
      case sle of
        Left err => pure $ Left err
        Right sl => 
            pure $ getReportForCourse course sl


display : Either String String -> IO ()
display (Left err) = putStrLn $ "Error: " ++ err
display (Right content) = putStrLn content

public export
write : Either String MD -> IO ()
write (Left err) = putStrLn $ "error: " ++ err
write (Right md) = do
  _ <- writeMD md
  putStrLn $ "Wrote: " ++ fromMaybe "" md.title

-- main : IO ()
-- main = do 
--   eReports <- traverse getReportByFile courseFiles
--   traverse_ write eReports
  
       

