import JSON.Simple.FromJSON
import Derive.Prelude
import Language.Reflection.Util

import JSON.Simple
import JSON.Simple.Derive

%language ElabReflection

record Foo where
  constructor MkFoo
  name : String
  id   : Maybe Int
  bool : Maybe Void

mb : Options
mb = { replaceMissingKeysWithNull := True } defaultOptions

singleOptions : Options
singleOptions = { sum := ObjectWithSingleField
                , replaceMissingKeysWithNull := True } defaultOptions

%runElab derive "Foo" [ Show, customFromJSON Export singleOptions, customToJSON Export singleOptions ] 


data Procedure = Baz String | Bip String

myOptions : Options
myOptions = { sum := ObjectWithSingleField } defaultOptions

%runElab derive "Procedure" [ Show, Eq, customToJSON Export singleOptions, customFromJSON Export singleOptions]

proc : Procedure
proc = Baz "walking"

procStr : String
procStr = encode proc

-- JSON.Simple.FromJSON.FromJSON Foo where
--   fromJSON =  withObject "Foo" $ \obj => do
--         name <- field obj "name"
--         id   <- fieldMaybe obj "id"
--         pure $ MkFoo name id
    
  
    -- name <- JSON.Simple.FromJSON.field json "name"
    -- id <- JSON.Simple.FromJSON.fieldMaybe json "id"
    -- pure $ MkFoo name id
  

g : Maybe Foo
g = decodeMaybe "{\"name\": \"George\", \"id\": 3}"

gg: Either JSON.Simple.FromJSON.DecodingErr Foo
gg = decode "{\"name\": \"George\"}"

-- j = parseJSON  Virtual "{\"name\": \"George\", \"id\": 3}"


-- h : Maybe String -> String
-- h ms = case ms of
--             Nothing => ?asdf_0
--             (Just x) => ?asdf_1


data Computation : Type where
  Value : String -> Computation
  Max   : String -> String -> Computation
  Min   : String -> String -> Computation
  Maxl  : List String -> Computation
  Minl  : List String -> Computation

%runElab derive "Computation" [ Show, Eq, customToJSON Export singleOptions, customFromJSON Export singleOptions]

