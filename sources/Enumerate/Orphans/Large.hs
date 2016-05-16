{-# LANGUAGE TypeFamilies, ExplicitNamespaces, DataKinds, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-| orphan instances, of 'Enumerable', for large types
(i.e. 'Word32' \/ 'Word64' \/ 'Int32' \/ 'Int64').

see:

* 'boundedEnumerated', 'boundedCardinality'

(that are included for completeness, but not exported by default
(i.e. by "Enumerate").
you probably want build-time instance-resolution errors instead of
probable runtime non-termination).

-}
module Enumerate.Orphans.Large where
import Enumerate.Types

import           Data.Word (Word32, Word64)
import           Data.Int (Int32, Int64)
-- import GHC.TypeLits (Nat, type (^))


{- | finite but too large. @2^64@ is a few billion.

>>> 1 + toInteger (maxBound::Int32) - toInteger (minBound::Int32)
4294967296

-}
instance Enumerable Int32  where
   -- type Cardinality Int32 = 4294967296 -- 2^32
   enumerated = boundedEnumerated
   cardinality = boundedCardinality

instance Enumerable Word32 where
  -- type Cardinality Word32 = 4294967296 -- 2^32
  enumerated = boundedEnumerated
  cardinality = boundedCardinality

{-| finite but too large. @2^64@ is over a billion billion.

e.g. 'Enumerate.reifyFunction' (which takes time linear in the domain)
on a function of type @(:: Int -> Bool)@,
won't terminate anytime soon.

>>> 1 + toInteger (maxBound::Int64) - toInteger (minBound::Int64)
18446744000000000000

-}
instance Enumerable Int64  where
   -- type Cardinality Int64 = 18446744000000000000 -- 2^64
   enumerated = boundedEnumerated
   cardinality = boundedCardinality

instance Enumerable Word64  where
   -- type Cardinality Word64 = 18446744000000000000 -- 2^64
   enumerated = boundedEnumerated
   cardinality = boundedCardinality

{-| finite but too large.

>>> 1 + toInteger (maxBound::Int) - toInteger (minBound::Int)
...

-}
instance Enumerable Int  where
   -- type Cardinality Int = INT_SIZE
   enumerated = boundedEnumerated
   cardinality = boundedCardinality

instance Enumerable Word  where
   -- type Cardinality Word = INT_SIZE -- ^ "A Word is an unsigned integral type, with the same size as Int."
   enumerated = boundedEnumerated
   cardinality = boundedCardinality

-- {-| size is platform-specific, often 2^32 or 2^64.
--
-- see <>
--
-- TODO find real size
--
-- -}
-- type INT_SIZE = 18446744000000000000
