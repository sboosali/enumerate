{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

--------------------------------------------------
--------------------------------------------------

{-|

-}

module Enumerate.Extra

 ( module Enumerate.Extra
 , module Prelude.Spiros
 ) where

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "containers" Data.Set as Set

--------------------------------------------------
--------------------------------------------------

-- import Language.Haskell.TH.Syntax (Name,nameBase)
import "base" Numeric.Natural

--------------------------------------------------

import qualified "base" Data.List as List
import qualified "base" Data.Ord  as Ord

--------------------------------------------------
--------------------------------------------------

import Prelude.Spiros hiding ((:*:),C) -- shadows GHC.Generics

--------------------------------------------------
--------------------------------------------------

nat2int :: Natural -> Int
nat2int = fromInteger . fromIntegral

--------------------------------------------------

int2natural :: Int -> Natural
int2natural = fromInteger . toInteger

--------------------------------------------------
--------------------------------------------------

{-| convert a power set to an isomorphic matrix, sorting the entries.

(for @doctest@)

-}

powerset2matrix :: Set (Set a) -> [[a]]
powerset2matrix = (List.sortBy (Ord.comparing length) . fmap Set.toList . Set.toList)

--------------------------------------------------

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

--------------------------------------------------

{-| >>> (powerset2matrix . dropEach . Set.fromList) [1..3]
[[1,2],[1,3],[2,3]]

-}

dropEach :: (Ord a) => Set a -> Set (Set a)
dropEach values = Set.map dropOne values

  where
  dropOne value = Set.delete value values

--------------------------------------------------
--------------------------------------------------

{-| 

>>> digitRange = rangeWith [1..10]
>>> digitRange (2,2)
[2]
>>> digitRange (2,3)
[2,3]
>>> digitRange (2,4)
[2,3,4]
>>> digitRange (0,10)
[]

--TODO:
-- >>> digitRange (4,2)
-- []

-}

rangeWith
  :: (Eq a)
  => [a] -> (a,a) -> [a]
rangeWith ordered = go
  where

  go (x, y)                     -- i.e. « (LowerBound, UpperBound) ».
    = if   x == y
      then [x]
      else ( appendUnlessEmpty y
           . takeWhile (/= y)
           . dropWhile (/= x)
           )
           ordered

  appendUnlessEmpty y xs = case xs of -- TODO hacky
          []    -> []
          (_:_) -> xs ++ [y] 

 -- . (++ [y])

{-# INLINE rangeWith #-}

--------------------------------------------------

--------------------------------------------------

-- {-| 

-- >>> betweenOneAndTen = withinWith [1..10]
-- >>> betweenOneAndTen 0
-- False
-- >>> betweenOneAndTen 10
-- True

-- -}

-- withinWith
--   :: (Eq a)
--   => [a] -> (a,a) -> a -> Bool
-- withinWith ordered = go
--   where

--   go (a, c) b
--     = _

-- {-# INLINE withinWith #-}

--------------------------------------------------
--------------------------------------------------