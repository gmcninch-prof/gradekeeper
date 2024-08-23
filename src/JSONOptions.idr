import Derive.Prelude
import JSON.Simple.Derive

  
singleOptions : Options
singleOptions = { sum := ObjectWithSingleField
                , constructorTagModifier := toLower 
                , replaceMissingKeysWithNull := True } defaultOptions


myToJSON   = customToJSON   Export singleOptions
myFromJSON = customFromJSON Export singleOptions

