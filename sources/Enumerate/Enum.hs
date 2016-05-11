{-# LANGUAGE ScopedTypeVariables #-}

{-|

usage:

@
data A = ...

instance 'Bounded' A where
 minBound = 'minBound_enumerable' array_A
 maxBound = 'maxBound_enumerable' array_A

instance 'Enum' A where
 toEnum   = 'toEnum_enumerable'   array_A
 fromEnum = 'fromEnum_enumerable' table_A

-- CAF
array_A :: 'Array' Int A
array_A = 'array_enumerable'

-- CAF
table_A :: 'Map' A Int
table_A = 'table_enumerable'

-- we must pass in <https://wiki.haskell.org/Constant_applicative_form CAF>s
-- (i.e. expressions that are top-level and unconstrained),
-- which will be shared between all calls to minBound/maxBound/toEnum/fromEnum.
-- TODO must we?
@

--TODO template-haskell

(also see the source of "Enumerate.Example")

-}
module Enumerate.Enum
 ( minBound_enumerable
 , maxBound_enumerable

 , toEnum_enumerable
 , fromEnum_enumerable

 , array_enumerable
 , table_enumerable
 ) where

import Enumerate.Types

import Numeric.Natural
import qualified Data.Array as Array --IntMap
import Data.Array (Array, (!))
import qualified Data.Map as Map
import Data.Map (Map)


minBound_enumerable :: forall a. (Enumerable a) => Array Int a -> a
minBound_enumerable as = (as ! 0) --TODO safe get:  (__fromJust__ "minBound")
{-# INLINE minBound_enumerable #-}

maxBound_enumerable :: forall a. (Enumerable a) => Array Int a -> a
maxBound_enumerable as = (as ! (n-1)) --TODO safe get:  (__fromJust__ "maxBound")
 where n = nat2int $ cardinality ([] :: [a])
{-# INLINE maxBound_enumerable #-}


toEnum_enumerable :: forall a. (Enumerable a) => Array Int a -> (Int -> a)
toEnum_enumerable as = \i -> (as ! i) -- i.e. (!) --TODO safe get:  (__fromJust__ "toEnum")
{-# INLINE toEnum_enumerable #-}

fromEnum_enumerable :: forall a. (Enumerable a, Ord a) => Map a Int -> (a -> Int)
fromEnum_enumerable as = \x -> (__fromJust__ "fromEnum") (Map.lookup x as)
{-# INLINE fromEnum_enumerable #-}


--TODO Nat ==> Int
array_enumerable :: forall a. (Enumerable a) => Array Int a --TODO
array_enumerable = Array.listArray (0, n - 1) enumerated --TODO is array efficient?
 where n = nat2int $ cardinality ([] :: [a])

table_enumerable :: forall a. (Enumerable a, Ord a) => Map a Int
table_enumerable = Map.fromList (zip enumerated [0 .. n - 1])
 where n = nat2int $ cardinality ([] :: [a])


__fromJust__ :: String -> Maybe a -> a
__fromJust__ name = maybe (__bug__ name) id

__bug__ :: String -> a
__bug__ name = error (name ++ ": invalid Enumerable instance")
--TODO print typerep; add constraint, all types are Typeable

nat2int :: Natural -> Int
nat2int = fromInteger . fromIntegral
