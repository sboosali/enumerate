{-# LANGUAGE RankNTypes #-}

--------------------------------------------------
--------------------------------------------------

{- | 

-}

module Enumerate.Function.Invert

  (
   -- * (doctest context)
   -- $setup

    module Enumerate.Function.Invert

  ) where

--------------------------------------------------
--------------------------------------------------

import Enumerate.Types

--------------------------------------------------

import Enumerate.Function.Extra
import Enumerate.Function.Types
import Enumerate.Function.Reify

--------------------------------------------------
--------------------------------------------------

import qualified Data.Map as Map
import           Data.Map (Map)

--------------------------------------------------

import qualified Data.Set as Set
import           Data.Set (Set)

--------------------------------------------------
--------------------------------------------------

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Semigroup                   ((<>))

import           Data.Maybe (fromJust, mapMaybe, listToMaybe)
import           Control.Arrow ((>>>))
import           Data.Function ((&))

--------------------------------------------------
-- DocTest ---------------------------------------
--------------------------------------------------

-- $setup
-- 
-- >>> :set +m
-- >>> :set -XLambdaCase
-- >>> import qualified Prelude
--

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{- | Convert a total function to a map.

>>> fromFunction not
fromList [(False,True),(True,False)]

(with Prelude 'not').

Morally, it's identity.

-}

fromFunction :: (Enumerable a, Ord a) => (a -> b) -> Map a b

fromFunction f = fromFunctionM (return.f)

{-# INLINABLE fromFunction #-}

--------------------------------------------------

{- | Convert a (safely-)partial function to a map.

Wraps 'reifyFunctionM'.

Morally, it's identity.

-}

fromFunctionM :: (Enumerable a, Ord a) => (Partial a b) -> Map a b

fromFunctionM f = Map.fromList (reifyFunctionM f)

{-# INLINABLE fromFunctionM #-}

--------------------------------------------------
--------------------------------------------------

{- | Convert the inverse of a (total) function to a map.

>>> fromInverse (==EQ)
fromList [(False,GT :| [LT]),(True,EQ :| [])]

('EQ' is from @Prelude@).

-}

fromInverse :: (Enumerable a, Ord a, Ord b) => (a -> b) -> (Map b (NonEmpty a))

fromInverse = fromFunction >>> invertMap

{-# INLINABLE fromInverse #-}

--------------------------------------------------

-- | See 'fromInverse'.

fromInverseM :: (Enumerable a, Ord a, Ord b) => (Partial a b) -> (Map b (NonEmpty a))

fromInverseM f = (fromFunctionM f) & invertMap

{-# INLINABLE fromInverseM #-}

--------------------------------------------------
--------------------------------------------------

{- | Convert the inverse of a (total) function to a map, assuming it is injective.

If not injective, only the "first" of the multiple outputs,
for any given input, is kept.

>>> fromInjective (== EQ)
fromList [(False,GT),(True,EQ)]

-}

fromInjective :: (Enumerable a, Ord a, Ord b) => (a -> b) -> (Map b a)

fromInjective = fromInverse >>> Map.map NonEmpty.head

{-# INLINABLE fromInjective #-}
  
--------------------------------------------------

-- | See 'fromInjective'.

fromInjectiveM :: (Enumerable a, Ord a, Ord b) => (Partial a b) -> (Map b a)

fromInjectiveM f = (fromInverseM f) & Map.map NonEmpty.head

{-# INLINABLE fromInjectiveM #-}
  
--------------------------------------------------
-- Mappings --------------------------------------
--------------------------------------------------

{-| Checks: "Does the map contain every key in its domain?".

>>> isMapTotal (Map.fromList [(False,True),(True,False)])
True

>>> isMapTotal (Map.fromList [('a',0)])
False

-}

isMapTotal
  :: (Enumerable a, Ord a)
  => Map a b -> Bool

isMapTotal m = all _isMember_ enumerated

  where
  _isMember_ x = x `Map.member` m

{-# INLINABLE isMapTotal #-}

--------------------------------------------------

-- {-| The coverage of a mapping. 

-- Checks: "What fraction of the range is covered by the mapping's codomain
-- (i.e. the @Map@'s values)?".

-- >>> getCoverageFraction (Map.fromList [(False,True),(True,False)])
-- 1 % 1

-- >>> getCoverageFraction (Map.fromList TODO ascii-mapping)
-- 1 % 1

-- -}

-- getCoverageFraction
--   :: (Enumerable a, Enumerable b, Ord a, Ord b)
--   => Map a b -> Ratio Natural

-- getCoverageFraction m = 
--   where
--   
--   q = m

-- {-# INLINABLE getCoverageFraction #-}

--------------------------------------------------
-- Sets: Input and Output ------------------------
--------------------------------------------------

{-| The domain of the (given, partial) function.

The <https://en.wikipedia.org/wiki/Partial_function#Basic_concepts domain>
of a function is: the subset of the 'enumerated' input where it's defined.

i.e. when @(x \`member\` 'domainM' f)@ holds, then @('fromJust' (f x))@ is defined.

>>> :{
let uppercasePartial :: (MonadThrow m) => Char -> m Char  -- :: Partial Char Char
    uppercasePartial = \case
     'a' -> return 'A'
     'b' -> return 'B'
     'z' -> return 'Z'
     _   -> failed "uppercasePartial"
in domainM uppercasePartial
:}
"abz"

-}

domainM :: (Enumerable a) => (Partial a b) -> [a]

domainM f = foldMap go enumerated

 where

 go a = case f a of
   Nothing -> []
   Just{}  -> [a]

{-# INLINABLE domainM #-}

--------------------------------------------------
--------------------------------------------------

{-| The co-range of the given function.

@'corange' _ = 'enumerated'
@

(TODO is this the right name?)

-}

corange :: (Enumerable a) => (a -> b) -> [a]
corange _ = enumerated

{-# INLINABLE corange #-}

--------------------------------------------------

{-| The co-range of the given function.

@'corangeM' _ = 'enumerated'
@

-}

corangeM :: (Enumerable a) => (Partial a b) -> [a]
corangeM _ = enumerated

{-# INLINABLE corangeM #-}

--------------------------------------------------
--------------------------------------------------

{-| The image of a (total) function.

@'imageM' f = 'fmap' f 'enumerated'
@

Includes duplicates.

-}

image :: (Enumerable a) => (a -> b) -> [b]
image f = map f enumerated

{-# INLINABLE image #-}

--------------------------------------------------

{-| The image of a (partial) function

NOTE The 'image' is a /not/ (necessarily) the 'codomain'.

@'imageM' f = 'mapMaybe' f 'enumerated'
@

Includes duplicates.

-}

imageM :: (Enumerable a) => (Partial a b) -> [b]
imageM f = mapMaybe f enumerated

{-# INLINABLE imageM #-}

--------------------------------------------------
--------------------------------------------------

{-| The co-domain of a function.

@'codomain' = 'const' 'enumerated'
@

See 'codomainM'.

-}

codomain :: (Enumerable b) => (a -> b) -> [b]
codomain _ = enumerated

{-# INLINABLE codomain #-}

--------------------------------------------------

{-| The co-domain of a function.

The co-domain contains the 'image'.

@'codomain' _ = 'enumerated'
@

NOTE the argument is ignored, at the value-level;
the function is a proxy, at the type-level, for its input type and output type.

-}

codomainM :: (Enumerable b) => (Partial a b) -> [b]
codomainM _ = enumerated

{-# INLINABLE codomainM #-}

--------------------------------------------------
-- Inverses --------------------------------------
--------------------------------------------------

{-| Invert a total function.

@(invert f) b@ is:

* @[]@ wherever @f@ is not surjective;
* @[y]@ wherever @f@ is uniquely-defined;
* @(_:_)@ wherever @f@ is not injective.

@'invert' f = 'invertM' ('return' '.' f)
@

-}

invert :: (Enumerable a, Ord a, Ord b) => (a -> b) -> (b -> [a])
invert f = invertM (return . f)

{-# INLINABLE invert #-}

--------------------------------------------------

{-| Invert a partial function.

@(invertM f) b@ is:

* @[]@ wherever @f@ is partial
* @[]@ wherever @f@ is not surjective
* @[y]@ wherever @f@ is uniquely defined
* @(_:_)@ wherever @f@ is not injective

a @Map@ is stored internally, with as many keys as the 'image' of @f@.

See also 'isBijectiveM'.

-}

invertM :: (Enumerable a, Ord a, Ord b) => (Partial a b) -> (b -> [a])
invertM f = g
 where
 g b = maybe [] NonEmpty.toList (Map.lookup b m)
 m = invertMap (fromFunctionM f) -- share the map

{-# INLINABLE invertM #-}
 
--------------------------------------------------

{-| Invert a total function, assuming injectivity.

@(invertInjection f) b@ is:

* @Nothing@ wherever @f@ is not surjective
* @Just y@ wherever @f@ is uniquely defined

@'invertInjection' f = 'invertInjectionM' ('return' '.' f)
@

Unlike 'isInjective', we silently ignore duplicates. i.e. when two inputs map to the same output, we pick the "first" (w.r.t. the arbitrary ordering of 'enumerated', or its reversal thereof). 

-}

invertInjection
  :: (Enumerable a, Ord a, Ord b)
  => (a -> b) -> (b -> Maybe a)

invertInjection f = invertInjectionM (return . f)

{-# INLINABLE invertInjection #-}

--------------------------------------------------

{-| See 'invertInjection'. 

-}

invertInjectionM
  :: (Enumerable a, Ord a, Ord b)
  => (Partial a b) -> (b -> Maybe a)

invertInjectionM f = g

  where

  g = f' >>> list2maybe

  f' = invertM f

{-# INLINABLE invertInjectionM #-}

--------------------------------------------------
--------------------------------------------------

{-| Check whether the (given) function is:

* 'Bijective', or
* 'Injective', or
* 'Surjective', or
* none of the above.

NOTE: Currently, the implementation is naive; it calls the validators
(i.e. 'isBijectiveM', 'isInjectiveM', 'isSurjectiveM') sequentially
(short-circuiting).

-}

getJectivityM
  :: (Enumerable a, Enumerable b, Ord a, Ord b)
  => (Partial a b) -> Maybe Jectivity

getJectivityM f

 = case isBijectiveM f of  -- TODO pick the right Monoid, whose append picks the first non-nothing
    Just{}  -> Just Bijective

    Nothing -> case isInjectiveM f of
                Just{}  -> Just Injective

                Nothing -> case isSurjectiveM f of
                            Just{}  -> Just Surjective

                            Nothing -> Nothing

--------------------------------------------------
-- Injective -------------------------------------
--------------------------------------------------

{-| Returns the inverse of the given (total) function,
if the given function is a injection.

@'isInjective' f = 'isInjectiveM' ('return' '.' f)
@

-}

isInjective :: (Enumerable a, Ord a, Ord b) => (a -> b) -> Maybe (b -> Maybe a)
isInjective f = isInjectiveM (return . f)

--------------------------------------------------

{-| Returns the inverse of the given (partial) function,
if the given function is a injection.

A function is injective if: its 'codomainM' 'isUnique'.

Refines @(b -> [a])@ (i.e. the type of 'invertM') to @(b -> Maybe a)@.

Unlike 'isBijectiveM', 'isInjectiveM' doesn't need an @('Enumerable' b)@ constraint.
This helps out when you want to ensure that a function into an infinite type
(e.g. 'show', the infinite type being @String@) is injective.
It's still reasonably efficient, given the @(Ord b)@ constraint.

-}

isInjectiveM :: (Enumerable a, Ord a, Ord b) => (Partial a b) -> Maybe (b -> Maybe a)
isInjectiveM f = do

  _bs <- isUnique (imageM f)   -- Map.fromListWith (<>) [(b, a:|[]) | (a, b) <- Map.toList m]
  return g

  -- TODO isInjectiveM: make it "correct by construction", rather than by explicit validation.

  where

  g = listToMaybe . invertM f
  -- can short-circuit.

--------------------------------------------------
-- Surjective ------------------------------------
--------------------------------------------------

{-| Returns the inverse of the given (total) function,
if the given function is a surjection.

@'isSurjective' f = 'isSurjectiveM' ('return' '.' f)
@

-}

isSurjective :: (Enumerable a, Enumerable b, Ord a, Ord b) => (a -> b) -> Maybe (b -> NonEmpty a)
isSurjective f = isSurjectiveM (return . f)

--------------------------------------------------

{-| Returns the inverse of the given (partial) function,
if the given function is a injection.

A function is surjective if: its codomain ('codomainM')
equals its image ('imageM').

Refines @(b -> [a])@ (i.e. the type of 'invertM') to @(b -> NonEmpty a)@.

Can short-circuit.

-}

isSurjectiveM :: (Enumerable a, Enumerable b, Ord a, Ord b) => (Partial a b) -> Maybe (b -> NonEmpty a)
isSurjectiveM f =

 if   _codmain_ `Set.isSubsetOf` _image_
      -- the reverse always holds, no need to check
 then Just g
 else Nothing

 where

 _codmain_ = Set.fromList (codomainM f)
 _image_   = Set.fromList (imageM    f)

 g = NonEmpty.fromList . invertM f
 -- safe, by validation

 -- TODO isSurjectiveM: make it "correct by construction", rather than explicit validation

--------------------------------------------------
-- Bijective -------------------------------------
--------------------------------------------------

{-| Returns the inverse of the given (total) function,
if the given function is a bijection.

@'isBijective' f = 'isBijectiveM' ('return' '.' f)
@

-}

isBijective :: (Enumerable a, Enumerable b, Ord a, Ord b) => (a -> b) -> Maybe (b -> a)
isBijective f = isBijectiveM (return . f)

--------------------------------------------------

{-| Returns the inverse of the bijection, if bijective.

A function is bijective if: it's both injective ('isInjectiveM')
and surjective ('isSurjectiveM').

Refines @(b -> [a])@ (i.e. the type of 'invertM') to @(b -> a)@.

Can short-circuit.

-}

isBijectiveM :: (Enumerable a, Enumerable b, Ord a, Ord b) => (Partial a b) -> Maybe (b -> a)
isBijectiveM f = do

 fIn    <- isInjectiveM f

 _fSur  <- isSurjectiveM f
 -- TODO avoid re-computing invertM. isInjectiveWithM isSurjectiveWithM

 let fBi = (fromJust . fIn)
 -- safe, because the intersection of "zero or one" with "one or more" is "one"

 return fBi

 -- let fOp = invertMap (fromFunctionM f) -- share the map

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

{-| Checks: "Is the list a valid set?".

Converts the (given) list into a set, if it has no duplicates.

NOTE: Currently, the implementation simply checks the 'length'
of the given list against the set built from that list.

-}

isUnique :: (Ord a) => [a] -> Maybe (Set a)
isUnique l =
  if   length l == length s
  then Just s
  else Nothing

 -- TODO make efficient, maybe single pass with Control.Foldl
 where

 s = Set.fromList l

--------------------------------------------------

{-| Invert any map, safely.

-}

invertMap :: (Ord a, Ord b) => Map a b -> Map b (NonEmpty a)
invertMap m = Map.fromListWith (<>) [(b, a:|[]) | (a, b) <- Map.toList m]

--------------------------------------------------
--------------------------------------------------