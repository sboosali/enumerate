{-# LANGUAGE RankNTypes #-}

--------------------------------------------------
--------------------------------------------------

{- | 

-}

module Enumerate.Function.Invert

  (
   -- * Doctest Context:
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
-- >>> :{
-- let uppercasePartial :: (MonadThrow m) => Char -> m Char  -- :: Partial Char Char
--     uppercasePartial = \case
--      'a' -> return 'A'
--      'b' -> return 'B'
--      'z' -> return 'Z'
--      _   -> failed "uppercasePartial"
-- :}
--
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

wraps 'reifyFunctionM'.

Morally, it's identity.

-}

fromFunctionM :: (Enumerable a, Ord a) => (Partial a b) -> Map a b
fromFunctionM f = Map.fromList (reifyFunctionM f)

{-# INLINABLE fromFunctionM #-}

--------------------------------------------------

{- | Convert the inverse of a total function to a map.

>>> fromInverse (==EQ)
fromList [(False,GT :| [LT]),(True,EQ :| [])]

(with Prelude 'EQ').

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

{- | Convert the inverse of a total function to a map, assuming it is injective.

if not injective, only the "first" of the multiple outputs for any given input is kept. 

>>> fromInjective (==EQ)
fromList [(False,GT),(True,EQ)]

(with Prelude 'EQ').

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

{-| Checks: "Does the map contain every key in its domain?".

>>> isMapTotal (Map.fromList [(False,True),(True,False)])
True

>>> isMapTotal (Map.fromList [('a',0)])
False

-}

isMapTotal :: (Enumerable a, Ord a) => Map a b -> Bool
isMapTotal m = all (\x -> Map.member x m) enumerated

--------------------------------------------------

{-| safely invert any map.

-}

invertMap :: (Ord a, Ord b) => Map a b -> Map b (NonEmpty a)
invertMap m = Map.fromListWith (<>) [(b, a:|[]) | (a, b) <- Map.toList m]

--------------------------------------------------

{-| the <https://en.wikipedia.org/wiki/Partial_function#Basic_concepts domain> of a partial function
is the subset of the 'enumerated' input where it's defined.

i.e. when @x \`member\` (domainM f)@ then @fromJust (f x)@ is defined.

>>> domainM uppercasePartial
"abz"

(with @uppercasePartial@ defined above).

-}

domainM :: (Enumerable a) => (Partial a b) -> [a]
domainM f = foldMap go enumerated
 where
 go a = case f a of
   Nothing -> []
   Just{}  -> [a]

--------------------------------------------------

{-| (right name?)

@corange _ = 'enumerated'@

-}

corange :: (Enumerable a) => (a -> b) -> [a]
corange _ = enumerated

--------------------------------------------------

{-|

@corangeM _ = 'enumerated'@

-}

corangeM :: (Enumerable a) => (Partial a b) -> [a]
corangeM _ = enumerated

--------------------------------------------------

{-| the image of a total function.

@imageM f = map f 'enumerated'@

includes duplicates.

-}

image :: (Enumerable a) => (a -> b) -> [b]
image f = map f enumerated

--------------------------------------------------

{-| the image (not the 'codomain') of a partial function.

@imageM f = mapMaybe f 'enumerated'@

includes duplicates.

-}

imageM :: (Enumerable a) => (Partial a b) -> [b]
imageM f = mapMaybe f enumerated

--------------------------------------------------

{-| the codomain of a function. it contains the 'image'.

@codomain _ = 'enumerated'@

-}

codomain :: (Enumerable b) => (a -> b) -> [b]
codomain _ = enumerated

--------------------------------------------------

codomainM :: (Enumerable b) => (Partial a b) -> [b]
codomainM _ = enumerated

--------------------------------------------------

{-| invert a total function.

@(invert f) b@ is:

* @[]@ wherever @f@ is not surjective
* @[y]@ wherever @f@ is uniquely defined
* @(_:_)@ wherever @f@ is not injective

@invert f = 'invertM' (return.f)@

-}

invert :: (Enumerable a, Ord a, Ord b) => (a -> b) -> (b -> [a])
invert f = invertM (return.f)

{-# INLINABLE invert #-}

--------------------------------------------------

{-| invert a partial function.

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

{-| invert a total function, assuming injectivity.

@(invertInjection f) b@ is:

* @Nothing@ wherever @f@ is not surjective
* @Just y@ wherever @f@ is uniquely defined

@invertInjection f = 'invertInjectionM' (return.f)@

Unlike 'isInjective', we silently ignore duplicates. i.e. when two inputs map to the same output, we pick the "first" (w.r.t. the arbitrary ordering of 'enumerated', or its reversal thereof). 

-}

invertInjection :: (Enumerable a, Ord a, Ord b) => (a -> b) -> (b -> Maybe a)
invertInjection f = invertInjectionM (return.f)
{-# INLINABLE invertInjection #-}

--------------------------------------------------

{-| See 'invertInjection'. 

-}

invertInjectionM :: (Enumerable a, Ord a, Ord b) => (Partial a b) -> (b -> Maybe a)
invertInjectionM f = g
 where
 g = f' >>> list2maybe -- NonEmpty.head  
 f' = invertM f

{-# INLINABLE invertInjectionM #-}

--------------------------------------------------

{-|

-}

getJectivityM :: (Enumerable a, Enumerable b, Ord a, Ord b) => (Partial a b) -> Maybe Jectivity
getJectivityM f
 = case isBijectiveM f of       -- TODO pick the right Monoid, whose append picks the first non-nothing
    Just{}  -> Just Bijective
    Nothing -> case isInjectiveM f of
                Just{}  -> Just Injective
                Nothing -> case isSurjectiveM f of
                            Just{}  -> Just Surjective
                            Nothing -> Nothing

--------------------------------------------------

{-| 
-}

isInjective :: (Enumerable a, Ord a, Ord b) => (a -> b) -> Maybe (b -> Maybe a)
isInjective f = isInjectiveM (return.f)

--------------------------------------------------

{-| Returns the inverse of the injection, if injective.

refines @(b -> [a])@ (i.e. the type of 'invertM') to @(b -> Maybe a)@.

unlike 'isBijectiveM', doesn't need an @(Enumerable b)@ constraint. this helps when you want to ensure a function into an infinite type (e.g. 'show') is injective. and still reasonably efficient, given the @(Ord b)@ constraint.

-}

isInjectiveM :: (Enumerable a, Ord a, Ord b) => (Partial a b) -> Maybe (b -> Maybe a)
isInjectiveM f = do             -- TODO make it "correct by construction", rather than explicit validation
 _bs <- isUnique (imageM f)   -- Map.fromListWith (<>) [(b, a:|[]) | (a, b) <- Map.toList m]
 return g
 where
 g = listToMaybe . invertM f
-- can short-circuit.

--------------------------------------------------

{-| Converts the list into a set, if it has no duplicates.

-}

isUnique :: (Ord a) => [a] -> Maybe (Set a)
isUnique l = if length l == length s then Nothing else Just s -- TODO make efficient, maybe single pass with Control.Foldl
 where
 s = Set.fromList l

--------------------------------------------------

{-| 
-}

isSurjective :: (Enumerable a, Enumerable b, Ord a, Ord b) => (a -> b) -> Maybe (b -> NonEmpty a)
isSurjective f = isSurjectiveM (return.f)

--------------------------------------------------

{-| Returns the inverse of the surjection, if surjective.
i.e. when a function's 'codomainM' equals its 'imageM'.

refines @(b -> [a])@ (i.e. the type of 'invertM') to @(b -> NonEmpty a)@.

can short-circuit.

-}

isSurjectiveM :: (Enumerable a, Enumerable b, Ord a, Ord b) => (Partial a b) -> Maybe (b -> NonEmpty a)
isSurjectiveM f =  -- TODO make it "correct by construction", rather than explicit validation
 if (Set.fromList (codomainM f)) `Set.isSubsetOf` (Set.fromList (imageM f))  -- the reverse always holds, no need to check
 then Just g
 else Nothing
 where
 g = NonEmpty.fromList . invertM f  -- safe, by validation

--------------------------------------------------

{-| 
-}

isBijective :: (Enumerable a, Enumerable b, Ord a, Ord b) => (a -> b) -> Maybe (b -> a)
isBijective f = isBijectiveM (return.f)

--------------------------------------------------

{-| Returns the inverse of the bijection, if bijective.

refines @(b -> [a])@ (i.e. the type of 'invertM') to @(b -> a)@.

can short-circuit.

-}

isBijectiveM :: (Enumerable a, Enumerable b, Ord a, Ord b) => (Partial a b) -> Maybe (b -> a)
isBijectiveM f = do
 fIn    <- isInjectiveM f
 _fSur  <- isSurjectiveM f --   TODO avoid re-computing invertM. isInjectiveWithM isSurjectiveWithM
 let fBi = (fromJust . fIn)  -- safe, because the intersection of "zero or one" with "one or more" is "one"
 return fBi
-- let fOp = invertMap (fromFunctionM f) -- share the map

--------------------------------------------------
--------------------------------------------------
