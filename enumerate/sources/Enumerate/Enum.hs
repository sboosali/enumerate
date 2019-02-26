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

 -- * @Ix@ methods:

 , range_Enumerable
 , unsafeIndex_Enumerable
 , inRange_Enumerable

 -- * Caching

 , array_Enumerable
 , table_Enumerable

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

--TODO IntMap
import qualified Data.Array as Array
import Data.Array (Array, (!))
import qualified Data.Map as Map
import Data.Map (Map)
import Prelude (error)

--------------------------------------------------
-- « Enum » --------------------------------------
--------------------------------------------------

toEnum_Enumerable :: forall a. (Enumerable a) => Array Int a -> (Int -> a)
toEnum_Enumerable as = \i -> (as ! i)

-- i.e. (!) --TODO safe get:  (__fromJust__ "toEnum")

{-# INLINE toEnum_Enumerable #-}

--------------------------------------------------

fromEnum_Enumerable :: forall a. (Enumerable a, Ord a) => Map a Int -> (a -> Int)
fromEnum_Enumerable as = \x -> (__fromJust__ "fromEnum") (Map.lookup x as)

{-# INLINE fromEnum_Enumerable #-}

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

{-# INLINE minBound_Enumerable' #-}

--TODO check core for sharing

--------------------------------------------------

maxBound_Enumerable' :: forall a. (Enumerable a) => a
maxBound_Enumerable' = maxBound_Enumerable array_Enumerable

{-# INLINE maxBound_Enumerable' #-}

--------------------------------------------------
--------------------------------------------------

toEnum_Enumerable' :: forall a. (Enumerable a) => (Int -> a)
toEnum_Enumerable'   = toEnum_Enumerable   array_Enumerable

{-# INLINE toEnum_Enumerable' #-}

--------------------------------------------------

fromEnum_Enumerable' :: forall a. (Enumerable a, Ord a) => (a -> Int)
fromEnum_Enumerable' = fromEnum_Enumerable table_Enumerable

{-# INLINE fromEnum_Enumerable' #-}

--------------------------------------------------
--------------------------------------------------

minBound_Enumerable :: forall a. (Enumerable a) => Array Int a -> a
minBound_Enumerable as = (as ! 0) --TODO safe get:  (__fromJust__ "minBound")

{-# INLINE minBound_Enumerable #-}

--------------------------------------------------

maxBound_Enumerable :: forall a. (Enumerable a) => Array Int a -> a

maxBound_Enumerable as = (as ! (n-1)) 
 where

 n = nat2int $ cardinality ([] :: [a])

--TODO safe get:  (__fromJust__ "maxBound")

{-# INLINE maxBound_Enumerable #-}

--------------------------------------------------
-- « Ix » ----------------------------------------
--------------------------------------------------

{- |

"The list of values in the subrange defined by a bounding pair."

-}

range_Enumerable
  :: forall a. (Enumerable a, Ord a)
  => (a,a) -> [a]

range_Enumerable = rangeWith enumerated

{-# INLINE range_Enumerable #-}

--------------------------------------------------

{- |

"Like 'index', but without checking that the value is in range."

-}

unsafeIndex_Enumerable
  :: forall a. (Enumerable a, Ord a)
  => (a,a) -> a -> Int

unsafeIndex_Enumerable = _indexWith _enumerated

{-# INLINE unsafeIndex_Enumerable #-}

--------------------------------------------------

{- | 

"Returns 'True' if the given subscript lies in the range defined
by the bounding pair."

-}

inRange_Enumerable
  :: forall a. (Enumerable a, Ord a)
  => (a,a) -> a -> Bool

inRange_Enumerable = _withinWith _enumerated

{-# INLINE inRange_Enumerable #-}

--------------------------------------------------
-- Caching ---------------------------------------
--------------------------------------------------

{-| 

Mapping from indices (@Int@) to values (@Enumerable a => @a@).

-}

array_Enumerable
 :: forall a. (Enumerable a)
 => Array Int a --TODO

array_Enumerable = array

  where

  array :: Array Int a
  array = Array.listArray bounds enumerated

  bounds :: (Int, Int)
  bounds = (0, n - 1)
  
  n = intCardinality ([] :: [a])

 --[TODO array_Enumerable] is array efficient?

--------------------------------------------------

{-| 

Mapping from enum values (@Enumerable a => a@) to their indices (@Int@).

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
-- Utilities -------------------------------------
--------------------------------------------------

__fromJust__ :: String -> Maybe a -> a
__fromJust__ name = maybe (__bug__ name) id

--------------------------------------------------

__bug__ :: String -> a
__bug__ name = error (name ++ ": invalid Enumerable instance")

--TODO print typerep; add constraint, all types are Typeable

--------------------------------------------------
{- Notes -----------------------------------------

from <http://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Arr.html#range>:

    {- ...
    Note [Inlining index]
    ~~~~~~~~~~~~~~~~~~~~~
    We inline the 'index' operation,
    
     * Partly because it generates much faster code
       (although bigger); see Trac #1216
    
     * Partly because it exposes the bounds checks to the simplifier which
       might help a big.
    
    If you make a per-instance index method, you may consider inlining it.
    
    Note [Double bounds-checking of index values]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    When you index an array, a!x, there are two possible bounds checks we might make:
    
      (A) Check that (inRange (bounds a) x) holds.
    
          (A) is checked in the method for 'index'
    
      (B) Check that (index (bounds a) x) lies in the range 0..n,
          where n is the size of the underlying array
    
          (B) is checked in the top-level function (!), in safeIndex.
    
    Of course it *should* be the case that (A) holds iff (B) holds, but that
    is a property of the particular instances of index, bounds, and inRange,
    so GHC cannot guarantee it.
    
     * If you do (A) and not (B), then you might get a seg-fault,
       by indexing at some bizarre location.  Trac #1610
    
     * If you do (B) but not (A), you may get no complaint when you index
       an array out of its semantic bounds.  Trac #2120
    
    At various times we have had (A) and not (B), or (B) and not (A); both
    led to complaints.  So now we implement *both* checks (Trac #2669).
    
    For 1-d, 2-d, and 3-d arrays of Int we have specialised instances to avoid this.
    
    Note [Out-of-bounds error messages]
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    The default method for 'index' generates hoplelessIndexError, because
    Ix doesn't have Show as a superclass.  For particular base types we
    can do better, so we override the default method for index.
    -}
    
    -- Abstract these errors from the relevant index functions so that
    -- the guts of the function will be small enough to inline.
    
    {-# NOINLINE indexError #-}
    indexError :: Show a => (a,a) -> a -> String -> b
    indexError rng i tp
      = errorWithoutStackTrace (showString "Ix{" . showString tp . showString "}.index: Index " .
               showParen True (showsPrec 0 i) .
               showString " out of range " $
               showParen True (showsPrec 0 rng) "")
    
    hopelessIndexError :: Int -- Try to use 'indexError' instead!
    hopelessIndexError = errorWithoutStackTrace "Error in array index"

-------------------------------------------------}