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

  putStrLn "\nenumerating domain...\n"
  traverse_ print (enumerated :: [Edit])

  putStrLn "\nserializing function...\n"
  putStrLn $ displayFunction emacsEdit

  putStrLn "\nserializing inverse...\n"
  traverse_ putStrLn $ displayInjective emacsEdit

  putStrLn "\ndetecting jectivity..."
  case isInjective emacsEdit of
    Nothing             -> print $ "`emacsEdit` is non-injective"
    Just fromKeyBinding -> do
      putStrLn $ "`emacsEdit` is injective:\n"

      let aKeyBinding = ["C-<spc>","M-b","C-w"]
      let anEdit = fromKeyBinding aKeyBinding
      putStrLn $ intercalate " " ["", show aKeyBinding, "<-", show anEdit]

data Edit = Edit Action Slice Region
 deriving (Show,Read,Eq,Ord,Generic,Enumerable)

data Action
 = Cut
 | Delete
 | Transpose
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

type KeyBinding = [String]

emacsEdit :: Edit -> KeyBinding
emacsEdit = \case
 Edit Transpose _     region -> emacsTranspose region
 Edit Cut       slice region -> emacsSelect region slice ++ ["C-w"]
 Edit Delete    slice region -> emacsSelect region slice ++ ["<del>"]

emacsTranspose :: Region -> KeyBinding
emacsTranspose = \case
 Character -> ["C-t"]
 Token     -> ["M-t"]
 Line      -> ["C-x-t"]

emacsSelect :: Region -> Slice -> KeyBinding
emacsSelect region = \case
 Whole     -> emacsBeginRegion region ++ emacsMark ++ emacsEndRegion region
 Backwards -> emacsMark ++ emacsBeginRegion region
 Forwards  -> emacsMark ++ emacsEndRegion region
 where
 emacsMark = ["C-<spc>"]

emacsBeginRegion :: Region -> KeyBinding
emacsBeginRegion = \case
 Character -> ["<left>"]
 Token     -> ["M-b"]
 Line      -> ["C-a"]

emacsEndRegion :: Region -> KeyBinding
emacsEndRegion = \case
 Character -> ["<right>"]
 Token     -> ["M-f"]
 Line      -> ["C-e"]
