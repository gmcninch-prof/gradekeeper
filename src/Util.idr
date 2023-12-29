module Util
import Data.String
import Data.Nat
import Data.Fin

import System.File.ReadWrite
import JSON.Derive


public export
hush : Either _ b -> Maybe b
hush (Left x) = Nothing
hush (Right x) = Just x


public export
decodefile : FromJSON a => String -> IO (Either String a)
decodefile filename = do 
  result <- readFile filename
  case result of
       (Left x)  => pure $ Left $ show x
       (Right x) => case decode  x of
                         (Left err) => pure $ Left $ show err
                         (Right y) => pure $ Right y  


infixr 5 ^

public export 
(^) : Double -> Nat -> Double
(^) dbl 0 = 1.0
(^) dbl (S k) = dbl * (dbl ^ k)

public export 
round' : Double -> Integer
round' dbl = if abs < 0.5 then floor else floor + 1
  where
    floor : Integer
    floor = cast dbl
    
    abs : Double
    abs = dbl - fromInteger floor

public export 
round : (prec : Nat) -> (num : Double) -> Double
round prec num = cast (round' $ num * mod ) / mod
  where
    mod : Double
    mod = 10^prec

public export 
remSpaces : String -> String
remSpaces str = 
  concat $ words str

