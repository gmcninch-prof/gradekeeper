module Util
import Data.String
import Data.Nat
import Data.Fin

import Data.Vect
import Data.Vect.Sort
import System.File.ReadWrite
import JSON.Simple.Derive


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


public export
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

export
mean : List Double -> Double
mean scores = sum scores / cast (length scores)


-- export
-- median : List Double -> Maybe Double
-- median items with (length (sort items))
--   median items | 0 = Nothing
--   median items | (S k) = do
--     let sitems = sort items
--     idx <- natToFin (div k 2) (length sitems)
--     pure $ index' sitems idx


data Mid a = Even a a
           | Odd a

middle : { a: Type} -> Vect (S n) a -> Mid a
middle (x :: []) = Odd x
middle (x :: (y :: [])) = Even x y
middle (x :: (y :: (z :: xs))) = middle $ init (y :: (z :: xs))


medianV : { a: Type} -> Ord a => Num a => Fractional a => {n:Nat} -> Vect (S n) a -> a
medianV {n} xs = case middle $ Data.Vect.Sort.sort xs of
                      (Even x y) => (x+y)/2
                      (Odd x) => x 


median1 : {a:Type} -> Ord a => Num a => Fractional a => List1 a -> a
median1 {a} xs = 
  case xs of
    (head ::: tail) => medianV $ fromList $ (head :: tail)

export
median : List Double -> Double
median [] = 0
median (x :: xs) = median1 (x ::: xs)
