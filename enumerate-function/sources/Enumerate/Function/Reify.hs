{-# LANGUAGE RankNTypes, LambdaCase #-}

--------------------------------------------------
--------------------------------------------------

{-| Reify a function into an explicit mapping between inputs and outputs.

See 'reifyFunctionAtM', the core definition
from which all other definitions (in this module) are derived.

NOTE Mappings are returned as association-lists, not @Map@s.

See the "Enumerate.Function.Map" module,
which imports @Enumerate.Function.Reify@,
and actually returns @Map@s (and @Set@s).

-}

module Enumerate.Function.Reify

  (
   -- * (doctest context)
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
-- Definitions: Core -----------------------------
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

Use the functions suffixed with @At@ (in this module):

* when your domain isn't 'Enumerable', or
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
i.e. has type 'Partial'), then try one of these (convenience) specializations:

* @'reifyFunctionMaybeAt'@: whene @(m ~ Maybe)@ .
* @'reifyFunctionListAt'@: whene @(m ~ [])@ .
* @'reifyFunctionEitherAt'@: whene @(m ~ Either SomeException)@ .

or:

* @'reifyFunctionSpoonAt'@: where @m@ is implicitly 'Identity',
while internally encapsulating @(m ~ 'IO')@.

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
-- Definitions: Derived --------------------------
--------------------------------------------------

{- | Reify a total function.

>>> reifyFunction Prelude.not
[(False,True),(True,False)]

@'reifyFunction' ≡ 'reifyFunctionAt' 'enumerated'
@

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

@'reifyFunctionM' ≡ 'reifyFunctionAtM' 'enumerated'
@

-}

reifyFunctionM
  :: (Enumerable a)
  => (forall m. MonadThrow m => a -> m b) -> [(a,b)]

reifyFunctionM = reifyFunctionAtM enumerated

{-# INLINABLE reifyFunctionM #-}

--------------------------------------------------
--------------------------------------------------

{- | Reify a (simple) predicate, on the given sub-domain.

A predicate is a safely-partial function that:

* fails via 'False'
* succeeds via 'True' (ignoring its output).

i.e. @('Partial' a b)@ where @(m ~ 'Identity')@ and @(b ~ '()')@.

'reifyPredicateAt' is included for completeness with 'reifyFunctionMaybeAt', etc.

w.r.t. the Output: @[a]@ represents an ordered @('Set' a)@, as
@[(a,b)]@ represents an ordered @('Map' a b)@.

NOTE The implementation is trivial:

@'reifyPredicateAt' = 'flip' 'filter'
@

-}

reifyPredicateAt :: [a] -> (a -> Bool) -> [a]
reifyPredicateAt = flip filter

{-# INLINABLE reifyPredicateAt #-}

--------------------------------------------------

-- | Reify a (safely-partial) function that fails specifically
-- under @Maybe@, on the given sub-domain.

reifyFunctionMaybeAt :: [a] -> (a -> Maybe b) -> [(a, b)]

reifyFunctionMaybeAt domain f = reifyFunctionAtM domain (maybe2throw f)

{-# INLINABLE reifyFunctionMaybeAt #-}

--------------------------------------------------

-- | Reify a (safely-partial) function that fails specifically
-- under @[]@, on the given sub-domain.

reifyFunctionListAt :: [a] -> (a -> [b]) -> [(a, b)]

reifyFunctionListAt domain f = reifyFunctionAtM domain (list2throw f)

{-# INLINABLE reifyFunctionListAt #-}

--------------------------------------------------

-- | Reify a (safely-partial) function that fails specifically
-- under @Either SomeException@, on the given sub-domain.

reifyFunctionEitherAt :: [a] -> (a -> Either SomeException b) -> [(a, b)]

reifyFunctionEitherAt domain f = reifyFunctionAtM domain (either2throw f)

{-# INLINABLE reifyFunctionEitherAt #-}

--------------------------------------------------
--------------------------------------------------

{- | Reify a predicate (on its whole domain).

@'reifyPredicate' = 'reifyPredicateAt' 'enumerated'
@

-}

reifyPredicate
  :: (Enumerable a)
  => (a -> Bool) -> [a]

reifyPredicate = reifyPredicateAt enumerated

{-# INLINABLE reifyPredicate #-}

--------------------------------------------------

{- | Reify a function that fails specifically under @Maybe@ (on its whole domain).

@'reifyFunctionMaybe' ≡ 'reifyFunctionMaybeAt' 'enumerated'
@

-}

reifyFunctionMaybe
  :: (Enumerable a)
  => (a -> Maybe b) -> [(a, b)]

reifyFunctionMaybe = reifyFunctionMaybeAt enumerated

{-# INLINABLE reifyFunctionMaybe #-}

--------------------------------------------------

{- | Reify a function that fails specifically under @[]@ (on its whole domain).

@'reifyFunctionList' ≡ 'reifyFunctionListAt' 'enumerated'
@

-}

reifyFunctionList
  :: (Enumerable a)
  => (a -> [b]) -> [(a, b)]

reifyFunctionList = reifyFunctionListAt enumerated

{-# INLINABLE reifyFunctionList #-}

--------------------------------------------------

{- | Reify a function that fails specifically under @Either SomeException@ (on its whole domain).

@'reifyFunctionEither' ≡ 'reifyFunctionEitherAt' 'enumerated'
@

-}

reifyFunctionEither
  :: (Enumerable a)
  => (a -> Either SomeException b) -> [(a, b)]

reifyFunctionEither = reifyFunctionEitherAt enumerated

{-# INLINABLE reifyFunctionEither #-}

--------------------------------------------------
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

{-# INLINABLE reifyFunctionSpoonAt #-}

--------------------------------------------------

{-| Reifies a total function, evaluating the range (completely).

Once the output mapping (i.e. the @[(a, b)]@),
is itself forced (i.e. completely-evaluated),
if @reifyFunctionSpoon@ returns (i.e. neither throws nor hangs),
then we've proved that the given function is, in fact, total
(modulo @unsafePerformIO@).

@'reifyFunctionSpoon' = 'reifyFunctionSpoonAt' 'enumerated'
@

-}

reifyFunctionSpoon
  :: ( NFData b, Enumerable a )
  => (a -> b) -> [(a, b)]

reifyFunctionSpoon = reifyFunctionSpoonAt enumerated

{-# INLINABLE reifyFunctionSpoon #-}

--------------------------------------------------
--------------------------------------------------

-- | Reify a binary total function.

reifyFunction2
  :: (Enumerable a, Enumerable b)
  => (a -> b -> c) -> [(a,[(b,c)])]

reifyFunction2 f = reifyFunction2At enumerated enumerated f

{-# INLINABLE reifyFunction2 #-}

--------------------------------------------------

-- | Reify a binary total function, at some sub-domain.

reifyFunction2At :: [a] -> [b] -> (a -> b -> c) -> [(a,[(b,c)])]

reifyFunction2At as bs f = reifyFunction2AtM as bs (\x y -> pure (f x y))

{-# INLINABLE reifyFunction2At #-}

--------------------------------------------------

-- | Reify a binary (safely-partial) function.

reifyFunction2M
  :: (Enumerable a, Enumerable b)
  => (forall m. MonadThrow m => a -> b -> m c) -> [(a,[(b,c)])]

reifyFunction2M f = reifyFunction2AtM enumerated enumerated f

{-# INLINABLE reifyFunction2M #-}

--------------------------------------------------

-- | Reify a binary (safely-partial) function, at some sub-domain.

reifyFunction2AtM
  :: [a] -> [b] -> (forall m. MonadThrow m => a -> b -> m c) -> [(a,[(b,c)])]

reifyFunction2AtM as bs f = reifyFunctionAt as (\a -> reifyFunctionAtM bs (f a))

--------------------------------------------------
--------------------------------------------------