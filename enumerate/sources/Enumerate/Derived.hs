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

-- import qualified Data.Array as Array
-- import Data.Array (Array, (!))

-- import qualified Data.Map as Map
-- import Data.Map (Map)

--------------------------------------------------

import "base" System.Timeout (timeout)

import "base" Data.Ix

--------------------------------------------------

--import Prelude (error)

--------------------------------------------------
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
