{-# LANGUAGE ScopedTypeVariables #-}
 
--------------------------------------------------
--------------------------------------------------

{-| Define (non-deriveable) 'Enum', 'Bounded', and 'Ix' instances
via a (deriveable) 'Enumerate' instance.

== Usage

@
data A = ...

instance 'Bounded' A where
 minBound = 'minBound_Enumerable' array_A
 maxBound = 'maxBound_Enumerable' array_A

instance 'Enum' A where
 toEnum   = 'toEnum_Enumerable'   array_A
 fromEnum = 'fromEnum_Enumerable' table_A

-- CAF
array_A :: 'Array' Int A
array_A = 'array_Enumerable'

-- CAF
table_A :: 'Map' A Int
table_A = 'table_Enumerable'

-- we must pass in <https://wiki.haskell.org/Constant_applicative_form CAF>s
-- (i.e. expressions that are top-level and unconstrained),
-- which will be shared between all calls to minBound/maxBound/toEnum/fromEnum.
-- TODO must we?
@

--TODO template-haskell

See, also, the source of "Enumerate.Example".

-}

--------------------------------------------------
--------------------------------------------------

module Enumerate.Enum

 (

 -- * @Enum@ methods:

   toEnum_Enumerable
 , fromEnum_Enumerable

 -- * @Bounded@ methods:

 , minBound_Enumerable
 , maxBound_Enumerable

 -- -- * @Ix@ methods:

 -- , range_Enumerable
 -- , unsafeIndex_Enumerable
 -- , inRange_Enumerable

 -- * Caching

 , table_Enumerable
 , index_Enumerable
 , array_Enumerable
 , sequence_Enumerable

 -- * Auxiliaries

 , minBound_Enumerable'
 , maxBound_Enumerable'

 , toEnum_Enumerable'
 , fromEnum_Enumerable'

 , toEnumDefault
 , fromEnumDefault

 ) where

--------------------------------------------------
--------------------------------------------------

import Enumerate.Types
import Enumerate.Extra

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

--------------------------------------------------

import qualified "containers" Data.IntMap as IntMap
import           "containers" Data.IntMap (IntMap)

--------------------------------------------------

import qualified "containers" Data.Sequence as Seq
import           "containers" Data.Sequence (Seq)

--------------------------------------------------

import qualified "array" Data.Array as Array
import           "array" Data.Array (Array, (!))

--------------------------------------------------

import qualified Prelude

--------------------------------------------------
-- « Enum » --------------------------------------
--------------------------------------------------

toEnum_Enumerable :: forall a. (Enumerable a) => Array Int a -> (Int -> a)
toEnum_Enumerable as = \i -> (as ! i)

-- i.e. (!) --TODO safe get:  (__fromJust__ "toEnum")

{-# INLINEABLE toEnum_Enumerable #-}

--------------------------------------------------

fromEnum_Enumerable :: forall a. (Enumerable a, Ord a) => Map a Int -> (a -> Int)
fromEnum_Enumerable as = \x -> (__fromJust__ "fromEnum") (Map.lookup x as)

{-# INLINEABLE fromEnum_Enumerable #-}

--------------------------------------------------
--------------------------------------------------

toEnumDefault :: forall a. (Enumerable a) => Int -> a
toEnumDefault = toEnum_Enumerable array_Enumerable
--NOTE use NOINLINE such that array_Enumerable is reused [TODO is this correct? check Core?]

{-# NOINLINE toEnumDefault #-}

--------------------------------------------------

fromEnumDefault :: forall a. (Enumerable a, Ord a) => a -> Int
fromEnumDefault = fromEnum_Enumerable table_Enumerable
-- use NOINLINE such that table_Enumerable is reused

{-# NOINLINE fromEnumDefault #-}

--------------------------------------------------
-- « Bounded » -----------------------------------
--------------------------------------------------

minBound_Enumerable' :: forall a. (Enumerable a) => a 
minBound_Enumerable' = minBound_Enumerable array_Enumerable

{-# INLINEABLE minBound_Enumerable' #-}

--TODO check core for sharing

--------------------------------------------------

maxBound_Enumerable' :: forall a. (Enumerable a) => a
maxBound_Enumerable' = maxBound_Enumerable array_Enumerable

{-# INLINEABLE maxBound_Enumerable' #-}

--------------------------------------------------
--------------------------------------------------

toEnum_Enumerable' :: forall a. (Enumerable a) => (Int -> a)
toEnum_Enumerable'   = toEnum_Enumerable   array_Enumerable

{-# INLINEABLE toEnum_Enumerable' #-}

--------------------------------------------------

fromEnum_Enumerable' :: forall a. (Enumerable a, Ord a) => (a -> Int)
fromEnum_Enumerable' = fromEnum_Enumerable table_Enumerable

{-# INLINEABLE fromEnum_Enumerable' #-}

--------------------------------------------------
--------------------------------------------------

minBound_Enumerable :: forall a. (Enumerable a) => Array Int a -> a
minBound_Enumerable as = (as ! 0) --TODO safe get:  (__fromJust__ "minBound")

{-# INLINEABLE minBound_Enumerable #-}

--------------------------------------------------

maxBound_Enumerable :: forall a. (Enumerable a) => Array Int a -> a

maxBound_Enumerable as = (as ! (n-1)) 
 where

 n = nat2int $ cardinality ([] :: [a])

--TODO safe get:  (__fromJust__ "maxBound")

{-# INLINEABLE maxBound_Enumerable #-}

--------------------------------------------------
-- « Ix » ----------------------------------------
--------------------------------------------------

-- {- |

-- "The list of values in the subrange defined by a bounding pair."

-- -}

-- range_Enumerable
--   :: forall a. (Enumerable a, Ord a)
--   => (a,a) -> [a]

-- range_Enumerable = rangeWith enumerated

-- {-# INLINEABLE range_Enumerable #-}

-- --------------------------------------------------

-- {- |

-- "Like 'index', but without checking that the value is in range."

-- -}

-- unsafeIndex_Enumerable
--   :: forall a. (Enumerable a, Ord a)
--   => (a,a) -> a -> Int

-- unsafeIndex_Enumerable = _indexWith _enumerated

-- {-# INLINEABLE unsafeIndex_Enumerable #-}

-- --------------------------------------------------

-- {- | 

-- "Returns @True@ if the given subscript lies in the range defined
-- by the bounding pair."

-- -}

-- inRange_Enumerable
--   :: forall a. (Enumerable a, Ord a)
--   => (a,a) -> a -> Bool

-- inRange_Enumerable = _withinWith _enumerated

-- {-# INLINEABLE inRange_Enumerable #-}

--------------------------------------------------
-- Caching ---------------------------------------
--------------------------------------------------

{-| Create a mapping from enum values (@('Enumerable' a) => a@) to their indices (@Int@),
given some type.

'table_Enumerable' is the “inverse“ of 'index_Enumerable'.

== Examples

>>> table_Bool = table_Enumerable :: Map Bool Int
>>> Map.toAscList table_Bool
[(False,0),(True,1)]

-}

table_Enumerable
  :: forall a. (Enumerable a, Ord a)
  => Map a Int

table_Enumerable = table

  where

  table :: Map a Int
  table = Map.fromList table'

  table' :: [(a, Int)]
  table' = zip enumerated indices

  indices :: [Int]
  indices = [0 .. (n - 1)]
  
  n = intCardinality ([] :: [a])

--------------------------------------------------

{-| Create a mapping from indices (@Int@) to values (@('Enumerable' a) => @a@),
given some type.

'index_Enumerable' is the “inverse“ of 'table_Enumerable'.

== Examples

>>> index_Bool = index_Enumerable :: IntMap Bool
>>> IntMap.toAscList index_Bool
[(0,False),(1,True)]

-}

index_Enumerable
 :: forall a. (Enumerable a)
 => IntMap a --TODO

index_Enumerable = index'IntMap

  where

  index'IntMap :: IntMap a
  index'IntMap = IntMap.fromList index'List

  index'List :: [(Int, a)]
  index'List = zip indices enumerated

  indices :: [Int]
  indices = [0 .. (n - 1)]
  
  n = intCardinality ([] :: [a])

--------------------------------------------------

{-| Create a mapping from indices (@Int@) to values (@('Enumerable' a) => @a@),
given some type.

== Examples

>>> array_Bool = array_Enumerable :: Array Int Bool
>>> array_Bool
array (0,1) [(0,False),(1,True)]

-}

array_Enumerable
 :: forall a. (Enumerable a)
 => Array Int a

array_Enumerable = array

  where

  array :: Array Int a
  array = Array.listArray bounds enumerated

  bounds :: (Int, Int)
  bounds = (0, n - 1)
  
  n = intCardinality ([] :: [a])

 --[TODO array_Enumerable] is array efficient?

--------------------------------------------------

{-| Create a mapping from indices (@Int@) to values (@('Enumerable' a) => @a@),
given some type.

== Examples

>>> sequence_Bool = sequence_Enumerable :: Seq Bool
>>> sequence_Bool
fromList [False,True]

-}

sequence_Enumerable
 :: forall a. (Enumerable a)
 => Seq a

sequence_Enumerable = seq'

  where

  seq' :: Seq a
  seq' = Seq.fromList enumerated

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

__fromJust__ :: String -> Maybe a -> a
__fromJust__ name = maybe (__bug__ name) id

--------------------------------------------------

__bug__ :: String -> a
__bug__ name = Prelude.error (name ++ ": invalid Enumerable instance")

--TODO print typerep; add constraint, all types are Typeable

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

{-
----------------------------------------

fromFunction :: Int -> (Int -> a) -> Seq a

O(n). Convert a given sequence length and a function representing that sequence into a sequence.

Since: containers-0.5.6.2

----------------------------------------

----------------------------------------
-}

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------