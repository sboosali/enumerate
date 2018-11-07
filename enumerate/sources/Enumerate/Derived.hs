--------------------------------------------------
--------------------------------------------------

{-| 

-}

--------------------------------------------------
--------------------------------------------------

module Enumerate.Derived where

--------------------------------------------------
--------------------------------------------------

import Enumerate.Types
import Enumerate.Extra

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

--TODO IntMap
import qualified Data.Array as Array
import Data.Array (Array, (!))
import qualified Data.Map as Map
import Data.Map (Map)
import Prelude (error)

--------------------------------------------------
--------------------------------------------------

{- | 

@'boundedEnumerated' = 'enumFromTo' 'minBound' 'maxBound'
@

= Usage

for non-'Generic' Bounded Enums:

@
instance Enumerable ... where
 'enumerated'  = 'boundedEnumerated'
 'cardinality' = 'boundedCardinality'
@

-}

boundedEnumerated :: (Bounded a, Enum a) => [a]
boundedEnumerated = enumFromTo minBound maxBound

{-# INLINE boundedEnumerated #-}

--------------------------------------------------

{-| for non-'Generic' Bounded Enums.

Assuming 'Bounded' is correct, safely stop the enumeration
(and know where to start).

behavior may be undefined when the cardinality of @a@ is larger than
the cardinality of @Int@. this should be okay, as @Int@ is at least as big as
@Int64@, which is at least as big as all the monomorphic types in @base@ that
instantiate @Bounded@. you can double-check with:

>>> boundedCardinality (const (undefined::Int))   -- platform specific
18446744073709551616

@
-- i.e. 1 + 9223372036854775807 - (-9223372036854775808)
@

Works with non-zero-based Enum instances, like @Int64@ or a custom
@toEnum/fromEnum@. Assumes the enumeration's numbering is
contiguous,. e.g. if @fromEnum 0@ and @fromEnum 2@
both exist, then @fromEnum 1@ should exist too.

-}

boundedCardinality
  :: forall proxy a. (Bounded a, Enum a)
  => proxy a -> Natural

boundedCardinality _ = fromInteger (1 + maxA - minA)

  where
  minA = (toInteger (fromEnum (minBound :: a)))
  maxA = (toInteger (fromEnum (maxBound :: a)))

{-# INLINE boundedCardinality #-}

--------------------------------------------------

{- | 

@'enumEnumerated' = 'enumFrom' ('toEnum' 0)
@

= Usage

for non-'Generic' Enums:

@
instance Enumerable ... where
 'enumerated' = 'enumEnumerated'
@

the enum should still be bounded.

WARNING the default 'cardinality' is slow
(@O(n)@ in the 'length' of 'enumerated').

-}

enumEnumerated :: (Enum a) => [a]
enumEnumerated = enumFrom (toEnum 0)

{-# INLINE enumEnumerated #-}

--------------------------------------------------

{- | 

= Usage

for non-'Generic' @Bounded@+@Indexed@ ('Ix') types:

@
instance Enumerable _ where
 'enumerated'  = indexedEnumerated
 'cardinality' = 'indexedCardinality'
@

-}

indexedEnumerated :: (Bounded a, Ix a) => [a]
indexedEnumerated = range (minBound,maxBound)

{-# INLINE indexedEnumerated #-}

--------------------------------------------------

{- | for non-'Generic' @Bounded@+@Indexed@ ('Ix') types.
-}

indexedCardinality :: forall proxy a. (Bounded a, Ix a) => proxy a -> Natural
indexedCardinality _ = int2natural (rangeSize (minBound, (maxBound::a)))

{-# INLINE indexedCardinality #-}

--------------------------------------------------

{-| Type-Cast 'cardinality'.

-}

intCardinality :: proxy a -> Natural
intCardinality proxy = nat2int (cardinality proxy)

{-# INLINE intCardinality #-}

--------------------------------------------------

{-| Enumerate only when the cardinality is small enough.
returns the cardinality when too large.

>>> enumerateBelow 2 :: Either Natural [Bool]
Left 2
>>> enumerateBelow 100 :: Either Natural [Bool]
Right [False,True]

Useful when you've established that traversing a list below some length
and consuming its values is reasonable for your application.
e.g. after benchmarking, you think you can process a billion entries
within a minute.

-}

enumerateBelow
  :: forall a. (Enumerable a)
  => Natural -> Either Natural [a] --TODO move

enumerateBelow maxSize =

  if   theSize `lessThan` maxSize
  then Right enumerated
  else Left theSize

  where
  theSize = cardinality (Proxy :: Proxy a)

{-# INLINE enumerateBelow #-}

--------------------------------------------------

{-| Enumerate only when completely evaluating the list doesn't timeout
(before the given number of microseconds).

>>> enumerateTimeout (2 * 10^6) :: IO (Maybe [Bool])  -- two seconds
Just [False,True]

-}

enumerateTimeout :: (Enumerable a, NFData a) => Int -> IO (Maybe [a]) --TODO move
enumerateTimeout maxDuration

  = timeout maxDuration (return $ force enumerated)

{-# INLINE enumerateTimeout #-}

--------------------------------------------------
--------------------------------------------------
