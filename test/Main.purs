module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)

import Data.Function.Named

-- If you have some function which takes a record as an argument...
f :: Named ( field1 :: Int, field2 :: Int ) Int
f = curryNamed \({ field1, field2 } :: { field1 :: Int, field2 :: Int}) ->
  field1 - field2

--f1 :: Named ( field2 :: Int ) Int
f1 :: { field2 :: Int } -> Int
f1 = f {field1: 1}

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow (f1 { field2: 2 })
