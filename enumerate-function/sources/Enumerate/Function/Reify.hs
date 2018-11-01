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
-- >>> import qualified Prelude
-- 

--------------------------------------------------
-- Definitions -----------------------------------
--------------------------------------------------

{- | Reify a total function.

>>> reifyFunction Prelude.not
[(False,True),(True,False)]

-}

reifyFunction
  :: (Enumerable a)
  => (a -> b) -> [(a,b)]

reifyFunction f = reifyFunctionM (return . f)

{-# INLINABLE reifyFunction #-}

--------------------------------------------------

-- | Reify a total function, at any (given) subset of the domain.

reifyFunctionAt :: [a] -> (a -> b) -> [(a,b)]

reifyFunctionAt theDomain f = reifyFunctionAtM theDomain (return . f)

{-# INLINABLE reifyFunctionAt #-}

--------------------------------------------------

{- | Reify a (safely-partial) function into a map. 

Any map is /implicitly/ partial, where @Map.lookup@ acts like @($)@.

-}

reifyFunctionM
  :: (Enumerable a)
  => (forall m. MonadThrow m => a -> m b) -> [(a,b)]

reifyFunctionM = reifyFunctionAtM enumerated

{-# INLINABLE reifyFunctionM #-}

--------------------------------------------------

{- | Reify a (safely-partial) function at any domain.

'reifyFunctionAtM' is the most general function in this module.

Use the functions suffixed with @M@ (in this module) when:

* your function is /explicitly partial/.

a.k.a /safely partial/, i.e. has a type like @(forall m. MonadThrow m => a -> m b)@.

The @Rank2@ type (and non-concrete types), when inside a function arrow like:

@
reifyFunctionAtM :: [a] -> (forall m. 'MonadThrow' m => a -> m b) -> [(a,b)]
reifyFunctionAtM domain f = ...
@

means that @f@ can only use either parametrically-polymorphic functions,
or the methods of the @MonadThrow@ class (namely 'throwM'),
or methods of @MonadThrow@ superclasses ('return', 'fmap', et cetera).

Use the functions suffixed with @At@ (in this module) when:

* your domain isn't 'Enumerable', or
* when you want to restrict the domain.

For example:

>>> :{
let uppercasePartial :: (MonadThrow m) => Char -> m Char  -- :: Partial Char Char
    uppercasePartial = \case
     'a' -> return 'A'
     'b' -> return 'B'
     'z' -> return 'Z'
     _   -> failed "uppercasePartial"
in reifyFunctionAtM ['a'..'c'] uppercasePartial
:}
[('a','A'),('b','B')]

If your function doesn't fail under 'MonadThrow' (i.e. via 'throwM',
i.e. has type 'Partial'), then see:

* 'reifyFunctionAtMaybe'
* 'reifyFunctionAtList'
* 'reifyFunctionAtEither'

NOTE 'MonadThrow' is a class from the @exceptions@ package that generalizes
failibility. it has instances for @Maybe@, @Either@, @[]@, @IO@, and more.

-}

reifyFunctionAtM :: [a] -> (Partial a b) -> [(a,b)]

reifyFunctionAtM domain f

  = concatMap (__bitraverse pure id)
  . fmap (id &&& f)
  $ domain

  where

  __bitraverse g h (x,y) = (,) <$> g x <*> h y  -- avoid bifunctors dependency

-- reifyFunctionAtM :: (MonadThrow m) => [a] -> (a -> m b) -> m (Map a b)

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

-- | Reify a (safely-partial) function that fails specifically under @Maybe@.

reifyFunctionMaybeAt :: [a] -> (a -> Maybe b) -> [(a, b)]

reifyFunctionMaybeAt domain f = reifyFunctionAtM domain (maybe2throw f)

{-# INLINABLE reifyFunctionMaybeAt #-}

--------------------------------------------------

-- | Reify a (safely-partial) function that fails specifically under @[]@.

reifyFunctionListAt :: [a] -> (a -> [b]) -> [(a, b)]

reifyFunctionListAt domain f = reifyFunctionAtM domain (list2throw f)

{-# INLINABLE reifyFunctionListAt #-}

--------------------------------------------------

-- | Reify a (safely-partial) function that fails specifically under @Either SomeException@.

reifyFunctionEitherAt :: [a] -> (a -> Either SomeException b) -> [(a, b)]

reifyFunctionEitherAt domain f = reifyFunctionAtM domain (either2throw f)

{-# INLINABLE reifyFunctionEitherAt #-}

--------------------------------------------------

{-| Reifies an *unsafely*-partial function.

An unsafely-partial (a.k.a. commonly called, simply, "partial function")
function is one that either:

* throws exceptions,
* has inexhaustive pattern matching,
* or loops indefinitely.

'reifyFunctionSpoonAt' catches exceptions, which deals with the latter two
(because a 'PatternMatchFail' exception is thrown by the runtime).

Forces the function to be strict.

>>> import Data.Ratio (Ratio)
>>> reciprocal = (1/)
>>> fmap reciprocal [0..3 :: Ratio Integer]
[*** Exception: Ratio has zero denominator
>>> reifyFunctionSpoonAt [0..3 :: Ratio Integer] reciprocal
[(1 % 1,1 % 1),(2 % 1,1 % 2),(3 % 1,1 % 3)]

NOTE The normal caveats apply:

* from violating purity (via @unsafePerformIO@)
* from catchalls (via @(e :: SomeExceptions -> _)@).

-}

reifyFunctionSpoonAt
  :: (NFData b)
  => [a] -> (a -> b) -> [(a, b)]

reifyFunctionSpoonAt domain f = reifyFunctionMaybeAt domain (totalizeFunction f)

--------------------------------------------------

-- | Reify a binary total function

reifyFunction2
  :: (Enumerable a, Enumerable b)
  => (a -> b -> c) -> [(a,[(b,c)])]

reifyFunction2 f = reifyFunction2At enumerated enumerated f

{-# INLINABLE reifyFunction2 #-}

--------------------------------------------------

-- | Reify a binary total function at some domain

reifyFunction2At :: [a] -> [b] -> (a -> b -> c) -> [(a,[(b,c)])]

reifyFunction2At as bs f = reifyFunction2AtM as bs (\x y -> pure (f x y))

{-# INLINABLE reifyFunction2At #-}

--------------------------------------------------

-- | Reify a binary (safely-partial) function

reifyFunction2M
  :: (Enumerable a, Enumerable b)
  => (forall m. MonadThrow m => a -> b -> m c) -> [(a,[(b,c)])]

reifyFunction2M f = reifyFunction2AtM enumerated enumerated f

{-# INLINABLE reifyFunction2M #-}

--------------------------------------------------

-- | Reify a binary (safely-partial) function at some domain

reifyFunction2AtM
  :: [a] -> [b] -> (forall m. MonadThrow m => a -> b -> m c) -> [(a,[(b,c)])]

reifyFunction2AtM as bs f = reifyFunctionAt as (\a -> reifyFunctionAtM bs (f a))

--------------------------------------------------
--------------------------------------------------