{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-| orphan instances, of 'Enumerate', for large types (i.e. 'Word32' \/ 'Word64' \/ 'Int32' \/ 'Int64').

(that are included for completeness, but not exported by default (i.e. by "Data.Enumerate").
you probably want build-time instance-resolution errors rather than probable runtime non-termination).

-}
module Data.Enumerate.Large where
import Data.Enumerate.Types

import           Data.Word (Word32, Word64)
import           Data.Int (Int32, Int64)


instance Enumerable Int32  where enumerated = boundedEnumerated; cardinality = boundedCardinality
instance Enumerable Word32 where enumerated = boundedEnumerated; cardinality = boundedCardinality

{-| finite but too big. @2^64@ is over a billion billion (@1,000,000,000,000@).

e.g. 'Enumerate.reifyFunction' (which takes time linear in the domain)
on a function of type @(:: Int -> Bool)@, even a lazy one,
won't terminate anytime soon.

-}
instance Enumerable Int64  where enumerated = boundedEnumerated; cardinality = boundedCardinality
instance Enumerable Word64 where enumerated = boundedEnumerated; cardinality = boundedCardinality
