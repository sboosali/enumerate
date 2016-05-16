{-# LANGUAGE DeriveAnyClass, DeriveGeneric, LambdaCase #-}
{-|

-}
module Enumerate.Function.Example where
import Enumerate.Types
import Enumerate.Function.Extra
import Enumerate.Function

{-
stack build && stack exec -- enumerate-function-example
-}
main = do

  putStrLn "\nenumerating..."
  _ <- traverse print (enumerated :: [Edit])

  putStrLn "\ndetecting jectivity..."
  print $ ""

data Edit = Edit Action Slice Region
 deriving (Show,Read,Eq,Ord,Generic,Enumerable)

data Action
 = Select
 | Copy
 | Delete
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)

data Slice
 = Whole
 | Backwards
 | Forwards
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)

data Region
 = Character
 | Token
 | Line
 deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic,Enumerable)

emacsEdit :: Edit -> String
emacsEdit = \case
 _ -> ""
