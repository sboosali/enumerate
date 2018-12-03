{-# LANGUAGE LambdaCase, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-| test that nullary method calls are shared. 

TODO cross-module benchmark and core check.

@
stack build --flag enumerate:dump-core && open core/Enumerate.Test.html
@

-}
module Enumerate.Test where
import Enumerate
import Prelude

import Data.Set (Set)
import GHC.Generics (Generic)

data B a
   = B0 a
   | B1 (Maybe a) (Either a a)
   | B2 (a, a)
   | B3 (Set a)
  deriving (Show,Eq,Ord,Generic,Enumerable)

instance Bounded (B Bool) where
 minBound = minBound_Enumerable' -- fast via laziness of lists, but arrays are spine-strict
 maxBound = maxBound_Enumerable' 

instance Enum (B Bool) where
 toEnum   = toEnum_Enumerable' 
 fromEnum = fromEnum_Enumerable' -- needs Ord

things = [minBound_Enumerable', maxBound_Enumerable', toEnum_Enumerable' 9] :: [B Bool]

