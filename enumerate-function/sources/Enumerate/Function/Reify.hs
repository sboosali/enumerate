{-# LANGUAGE RankNTypes, LambdaCase #-}

--------------------------------------------------
--------------------------------------------------

{-| See 'reifyFunctionAtM'.

-}

module Enumerate.Function.Reify

  (
   -- * Doctest Context:
   -- $setup

    module Enumerate.Function.Reify

  ) where

--------------------------------------------------
-- Imports: (Internal) Project Libraries ---------
--------------------------------------------------

import Enumerate.Types

--------------------------------------------------

import Enumerate.Function.Types
import Enumerate.Function.Extra

--------------------------------------------------
-- Imports: (External) Dependency Libraries ------
--------------------------------------------------

import "exceptions" Control.Monad.Catch (MonadThrow(..), SomeException(..))

--------------------------------------------------

import "deepseq"    Control.DeepSeq (NFData)

--------------------------------------------------
-- Imports: Standard Library ---------------------
--------------------------------------------------

import Control.Arrow ((&&&))

--------------------------------------------------

-- import GHC.TypeLits (Nat, type (^))

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

{- | Reify a total function.

>>> reifyFunction not
[(False,True),(True,False)]

(with @Prelude@ 'not').

-}

reifyFunction :: (Enumerable a) => (a -> b) -> [(a,b)]
reifyFunction f = reifyFunctionM (return . f)

{-# INLINABLE reifyFunction #-}

--------------------------------------------------

-- | Reify a total function at any subset of the domain.
reifyFunctionAt :: [a] -> (a -> b) -> [(a,b)]
reifyFunctionAt domain f = reifyFunctionAtM domain (return . f)

{-# INLINABLE reifyFunctionAt #-}

--------------------------------------------------

-- | Reify a (safely-)partial function into a map (which is implicitly partial, where @Map.lookup@ is like @($)@.
reifyFunctionM :: (Enumerable a) => (forall m. MonadThrow m => a -> m b) -> [(a,b)]
reifyFunctionM = reifyFunctionAtM enumerated

{-# INLINABLE reifyFunctionM #-}

--------------------------------------------------

{- | Reify a (safely-)partial function at any domain.

use the functions suffixed with @M@ when your function is explicitly partial,
i.e. of type @(forall m. MonadThrow m => a -> m b)@.
when inside a function arrow, like:

@
reifyFunctionAtM :: [a] -> (forall m. MonadThrow m => a -> m b) -> [(a,b)]
reifyFunctionAtM domain f = ...
@

the @Rank2@ type (and non-concrete types) means that @f@ can only use
parametric polymorphic functions, or the methods of the @MonadThrow@ class
(namely 'throwM'), or methods of @MonadThrow@ superclasses (namely 'return', et cetera).

'MonadThrow' is a class from the @exceptions@ package that generalizes failibility.
it has instances for @Maybe@, @Either@, @[]@, @IO@, and more.

use the functions suffixed with @At@ when your domain isn't 'Enumerable',
or when you want to restrict the domain.

the most general function in this module.

>>> reifyFunctionAtM ['a'..'c'] uppercasePartial
[('a','A'),('b','B')]

(with @uppercasePartial@ defined above).

if your function doesn't fail under 'MonadThrow', see:

* 'reifyFunctionAtMaybe'
* 'reifyFunctionAtList'
* 'reifyFunctionAtEither'

-}

reifyFunctionAtM :: [a] -> (Partial a b) -> [(a,b)]
-- reifyFunctionAtM :: (MonadThrow m) => [a] -> (a -> m b) -> m (Map a b)
reifyFunctionAtM domain f
 = concatMap (__bitraverse pure id)
 . fmap (id &&& f)
 $ domain
 where
 __bitraverse g h (x,y) = (,) <$> g x <*> h y  -- avoid bifunctors dependency

--------------------------------------------------

-- | @reifyPredicateAt = 'flip' 'filter'@
reifyPredicateAt :: [a] -> (a -> Bool) -> [a]
reifyPredicateAt = flip filter

-- reifyPredicateAtM domain p = map fst (reifyFunctionAtM domain f)
--  where
--  f x = if p x then return x else throwM (ErrorCall "False")

-- MonadThrow Maybe
-- (e ~ SomeException) => MonadThrow (Either e)
-- MonadThrow []

--------------------------------------------------

-- | Reify a (safely-)partial function that fails specifically under @Maybe@.
reifyFunctionMaybeAt :: [a] -> (a -> Maybe b) -> [(a, b)]
reifyFunctionMaybeAt domain f = reifyFunctionAtM domain (maybe2throw f)

{-# INLINABLE reifyFunctionMaybeAt #-}

--------------------------------------------------

-- | Reify a (safely-)partial function that fails specifically under @[]@.
reifyFunctionListAt :: [a] -> (a -> [b]) -> [(a, b)]
reifyFunctionListAt domain f = reifyFunctionAtM domain (list2throw f)

{-# INLINABLE reifyFunctionListAt #-}

--------------------------------------------------

-- | Reify a (safely-)partial function that fails specifically under @Either SomeException@.
reifyFunctionEitherAt :: [a] -> (a -> Either SomeException b) -> [(a, b)]
reifyFunctionEitherAt domain f = reifyFunctionAtM domain (either2throw f)

{-# INLINABLE reifyFunctionEitherAt #-}

--------------------------------------------------

{-| Reifies an *unsafely*-partial function (i.e. a function that throws exceptions or that has inexhaustive pattern matching).

Forces the function to be strict.

(TODO these hang) For example:

>>> import Data.Ratio (Ratio)
>>> reciprocal = (1/)
>>> fmap reciprocal [0..3 :: Ratio Integer]
[*** Exception: Ratio has zero denominator
>>> reifyFunctionSpoonAt [0..3 :: Ratio Integer] reciprocal
[(1 % 1,1 % 1),(2 % 1,1 % 2),(3 % 1,1 % 3)]

Normal caveats apply, from violating purity (via @unsafePerformIO@) and from catchalls (via @(e :: SomeExceptions -> _)@).

-}

reifyFunctionSpoonAt :: (NFData b) => [a] -> (a -> b) -> [(a, b)]
reifyFunctionSpoonAt domain f = reifyFunctionMaybeAt domain (totalizeFunction f)

--------------------------------------------------

-- | Reify a binary total function
reifyFunction2 :: (Enumerable a, Enumerable b) => (a -> b -> c) -> [(a,[(b,c)])]
reifyFunction2 f = reifyFunction2At enumerated enumerated f

{-# INLINABLE reifyFunction2 #-}

--------------------------------------------------

-- | Reify a binary total function at some domain
reifyFunction2At :: [a] -> [b] -> (a -> b -> c) -> [(a,[(b,c)])]
reifyFunction2At as bs f = reifyFunction2AtM as bs (\x y -> pure (f x y))

{-# INLINABLE reifyFunction2At #-}

--------------------------------------------------

-- | Reify a binary (safely-)partial function
reifyFunction2M :: (Enumerable a, Enumerable b) => (forall m. MonadThrow m => a -> b -> m c) -> [(a,[(b,c)])]
reifyFunction2M f = reifyFunction2AtM enumerated enumerated f

{-# INLINABLE reifyFunction2M #-}

--------------------------------------------------

-- | Reify a binary (safely-)partial function at some domain
reifyFunction2AtM :: [a] -> [b] -> (forall m. MonadThrow m => a -> b -> m c) -> [(a,[(b,c)])]
reifyFunction2AtM as bs f = reifyFunctionAt as (\a -> reifyFunctionAtM bs (f a))

--------------------------------------------------
--------------------------------------------------