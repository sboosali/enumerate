{-# LANGUAGE ScopedTypeVariables #-}
 
--------------------------------------------------
--------------------------------------------------

{-| Define (non-deriveable) 'Ix' instances
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


@
inRange (l,u) i == elem i (range (l,u))
range (l,u) !! index (l,u) i == i, when inRange (l,u) i
map (index (l,u)) (range (l,u))) == [0..rangeSize (l,u)-1]
rangeSize (l,u) == length (range (l,u))
@

-}

--------------------------------------------------
--------------------------------------------------

module Enumerate.Ix

 (
 -- * @Ix@ methods:

   range_Enumerable
 , unsafeIndex_Enumerable
 , inRange_Enumerable

 ) where

--------------------------------------------------
--------------------------------------------------

import Enumerate.Prelude

import Enumerate.Types
import Enumerate.Enum

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import qualified "containers" Data.Map as Map
import           "containers" Data.Map (Map)

--------------------------------------------------

import qualified "containers" Data.Sequence as Seq
import           "containers" Data.Sequence (Seq)

--------------------------------------------------

import qualified Prelude

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

--type BinarySearchTree a = (Int,a) -- TODO

-- Data.Map
-- Data.IntMap
-- Data.Sequence

--------------------------------------------------
-- « Ix » ----------------------------------------
--------------------------------------------------

{- | A (`Seq`-based) `range` implementation for an 'Enumerable' type.

"The list of values in the subrange defined by a bounding pair."

-}

range_Enumerable
  :: forall a. (Enumerable a, Ord a)
  => (a,a) -> [a]

range_Enumerable = range_Seq sequence_Enumerable

{-# INLINEABLE range_Enumerable #-}

--------------------------------------------------

{-

{- |

"Like 'index', but without checking that the value is in range."

-}

unsafeIndex_Enumerable
  :: forall a. (Enumerable a, Ord a)
  => (a,a) -> a -> Int

unsafeIndex_Enumerable = _indexWith _enumerated

{-# INLINEABLE unsafeIndex_Enumerable #-}

--------------------------------------------------

{- | 

"Returns @True@ if the given subscript lies in the range defined
by the bounding pair."

-}

inRange_Enumerable
  :: forall a. (Enumerable a, Ord a)
  => (a,a) -> a -> Bool

inRange_Enumerable = _withinWith _enumerated

{-# INLINEABLE inRange_Enumerable #-}

--------------------------------------------------
-- Caching ---------------------------------------
--------------------------------------------------

{-| Binary Search Tree for all values in a type.

Efficiently find subsets of 

Mapping from indices (@Int@) to values (@Enumerable a => @a@).

-}

tree_Enumerable
 :: forall a. (Enumerable a)
 => BinarySearchTree a

tree_Enumerable = tree

  where

  tree :: Tree Int a
  tree = Tree.listTree bounds enumerated

  bounds :: (Int, Int)
  bounds = (0, n - 1)
  
  n = intCardinality ([] :: [a])

-}

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

{- | Return a `range` function, given a `Seq`uence.

i.e. @range_Seq xs@ asssumes the `Seq`uence @(xs :: 'Seq' a)@ is monotonically increasing,
w.r.t @('Ord' a)@ . You may call 'Seq.sort' on @xs@ to ensure this prerequisite.

-}

range_Seq
  :: forall a. (Ord a)
  => Seq a
  -> ((a,a) -> [a])

range_Seq xs = range_
  where

  range_ :: (a,a) -> [a]
  range_ (i,k) =

    if   (i <= k)
    then toList js
    else []

    where

      js = xs &
        ( Seq.dropWhileL (`lessThan` i)
        > Seq.takeWhileL (<= k)
        )

{-# INLINEABLE range_Seq #-}

--------------------------------------------------

{- | Return an `inRange` function, given a `Seq`uence.

-}

inRange_Seq
  :: forall a. (Ord a)
  => Seq a
  -> ((a, a) -> a -> Bool)

inRange_Seq xs = inRange_
  where

  inRange_ :: (a, a) -> a -> Bool
  inRange_ (i,j) x = ys
    where

      ys = _ xs

{-# INLINEABLE inRange_Seq #-}

--------------------------------------------------
-- Notes -----------------------------------------
--------------------------------------------------

{-

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

-}

--------------------------------------------------
-- EOF -------------------------------------------
--------------------------------------------------