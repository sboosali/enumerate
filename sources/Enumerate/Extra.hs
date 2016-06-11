{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}
{-|

-}
module Enumerate.Extra
 ( module Enumerate.Extra
 , module Prelude.Spiros
 ) where


-- import Language.Haskell.TH.Syntax (Name,nameBase)
import Numeric.Natural
import qualified Data.Set as Set
import qualified Data.List as List
import qualified Data.Ord as Ord
import Prelude.Spiros hiding ((:*:),C) -- shadows GHC.Generics


int2natural :: Int -> Natural
int2natural = fromInteger . toInteger

{-| the power set of a set of values.

>>> (powerset2matrix . powerSet . Set.fromList) [1..3]
[[],[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]]

-}
powerSet :: (Ord a) => Set a -> Set (Set a) --TODO use [[a]]
powerSet values =
   Set.singleton values `Set.union` _Set_bind powerSet (dropEach values)
 where
 _Set_bind :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
 _Set_bind f = _Set_join . Set.map f
 _Set_join :: (Ord a) => Set (Set a) -> Set a
 _Set_join = Set.unions . Set.toList

{-| >>> (powerset2matrix . dropEach . Set.fromList) [1..3]
[[1,2],[1,3],[2,3]]

-}
dropEach :: (Ord a) => Set a -> Set (Set a)
dropEach values = Set.map dropOne values
 where
 dropOne value = Set.delete value values

{-| convert a power set to an isomorphic matrix, sorting the entries.

(for doctest)

-}
powerset2matrix :: Set (Set a) -> [[a]]
powerset2matrix = (List.sortBy (Ord.comparing length) . fmap Set.toList . Set.toList)

{-| (for doctest)
-}
printMappings :: (Show a) => [[a]] -> IO ()
printMappings mappings = traverse_ (\mapping -> (putStrLn"") >> (traverse print) mapping) mappings >> return()
