module Process

import System.File.ReadWrite
import JSON.Derive

import Course
import Student
import MD
import Reports 

%language ElabReflection

decodefile : Show a => FromJSON a => String -> IO (Either String a)
decodefile filename = do 
  result <- readFile filename
  case result of
       (Left x)  => pure $ Left $ show x
       (Right x) => case decode  x of
                         (Left err) => pure $ Left $ show err
                         (Right y) => pure $ Right y  



dir : String
dir = "/home/george/Prof-Teach/scores/"

courseFile : String
courseFile = String.joinBy "/" [ dir, "AY2021-2022--2022-sp--Math051.json" ]


course : IO (Either String Course)
course = decodefile courseFile

explain : { a: Type} -> (msg : String) -> Maybe a -> Either String a
explain msg Nothing = Left msg
explain msg (Just x) = Right x


computeResults : Course -> List StudentSISData -> Either String (List StudentResult)
computeResults course students = do
  explain errmsg $ traverse (result course) students
  
  where
    errmsg : String
    errmsg = "Failed to construct student results"



getData : (courseFileName : String) -> IO (Either String (Course,List StudentSISData))
getData courseFileName = do
  ce <- decodefile courseFileName
  case ce of
    Left err => pure $ Left err
    Right course => do
      sle <- decodefile $ String.joinBy "/" [ course.dataDir, course.SISFile ]
      case sle of
        Left err => pure $ Left err
        Right sl => 
            pure $ Right (course,sl)


getReport : Course -> List StudentSISData -> Either String String
getReport course students = do
  results <- computeResults course students
  
  let rpt : MD
      rpt = mkCourseReport course results
      
  pure $ render rpt

display : Either String String -> IO ()
display (Left err) = putStrLn $ "Error: " ++ err
display (Right content) = putStrLn content


main : IO ()
main = do 
  dat  <- getData courseFile
  case dat of
    Left err => putStrLn err
    Right (course,studentdata) =>
      display $ getReport course studentdata
      
      
 -- c <- decodefile courseFile     
  -- s <- decodefile $ String.joinBy "/" [ c.dataDir, c.SISFile ]

  -- case report c s of
  --   (Left err) => putStrLn err
  --   (Right x) => putStrLn x
    
       

