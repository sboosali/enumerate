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
--------------------------------------------------

import qualified "containers" Data.Set as Set

--------------------------------------------------
--------------------------------------------------

-- import Language.Haskell.TH.Syntax (Name,nameBase)
import Numeric.Natural

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