module Test

import Derive.Prelude
import JSON.Derive

fooOptions : Options
fooOptions = { sum := ObjectWithSingleField
             , constructorTagModifier := toLower } defaultOptions


%language ElabReflection

export 
data Foo : Type where
  A : ( label : String) -> Foo
  B : ( label : String) -> ( ack :  String) -> Foo
  C : ( label : String) -> String -> Int -> Foo

%runElab derive "Foo" [ Show, Eq, customToJSON fooOptions, customFromJSON fooOptions]


foos : List Foo
foos = [ A {label = "l1"}, B "l" "bar", C "l3" "baz" 3, C "l4" "bazz" 8 ]
