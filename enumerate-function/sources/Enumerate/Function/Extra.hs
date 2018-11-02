{-# LANGUAGE RankNTypes, LambdaCase, ScopedTypeVariables #-}

--------------------------------------------------
--------------------------------------------------

module Enumerate.Function.Extra

 ( module Enumerate.Function.Extra
 , module Prelude.Spiros

 , module Control.DeepSeq
 , module Data.Semigroup

 , module Control.Arrow
 , module Data.Function
 , module Data.List
 , module Data.Foldable

 , module Data.Data
 , module GHC.Generics

 ) where

--------------------------------------------------
--------------------------------------------------

import "deepseq"    Control.DeepSeq     (NFData(..), deepseq)

--------------------------------------------------

import "exceptions" Control.Monad.Catch (MonadThrow(..), SomeException(..))

--------------------------------------------------
--------------------------------------------------

import Data.Semigroup (Semigroup)

import System.IO.Unsafe (unsafePerformIO)

import Control.Exception
  ( Handler(..)
  , AsyncException, ArithException, ArrayException, ErrorCall, PatternMatchFail
  , catches, throwIO
  )

--------------------------------------------------
--------------------------------------------------

import qualified "containers" Data.Set as Set

--------------------------------------------------
--------------------------------------------------

import "base" Control.Arrow ((>>>),(<<<))
import "base" Data.Function ((&))
import "base" Data.List (intercalate)
import "base" Data.Foldable (traverse_)
import "base" Data.Data (Data)

--------------------------------------------------

import qualified "base" Data.List as List
import qualified "base" Data.Ord  as Ord

--------------------------------------------------

import "base" GHC.Generics (Generic)

--------------------------------------------------
--------------------------------------------------

import "spiros" Prelude.Spiros

--------------------------------------------------
--------------------------------------------------

{-| @failed = 'throwM' . 'userError'@

-}

failed ::  (MonadThrow m) => String -> m a
failed = throwM . userError

--------------------------------------------------

-- | generalize a function that fails with @Nothing@.
maybe2throw :: (a -> Maybe b) -> (forall m. MonadThrow m => a -> m b)
maybe2throw f = f >>> \case
 Nothing -> failed "Nothing"
 Just x  -> return x

--------------------------------------------------

-- | generalize a function that fails with @[]@.
list2throw :: (a -> [b]) -> (forall m. MonadThrow m => a -> m b)
list2throw f = f >>> \case
 []    -> failed "[]"

 (x:_) -> return x

--------------------------------------------------

-- | generalize a function that fails with @Left@.
either2throw :: (a -> Either SomeException b) -> (forall m. MonadThrow m => a -> m b)
either2throw f = f >>> \case
 Left  e -> throwM e
 Right x -> return x

--------------------------------------------------

{-| specialization -}
throw2maybe :: (forall m. MonadThrow m => a -> m b) -> (a -> Maybe b)
throw2maybe = id

--------------------------------------------------

{-| specialization -}
throw2either :: (forall m. MonadThrow m => a -> m b) -> (a -> Either SomeException b)
throw2either = id

--------------------------------------------------

{-| specialization -}
throw2list :: (forall m. MonadThrow m => a -> m b) -> (a -> [b])
throw2list = id

--------------------------------------------------
--------------------------------------------------

{-| makes an *unsafely*-partial function

(i.e. a function that throws exceptions or
that has inexhaustive pattern matching)
into a *safely*-partial function
(i.e. that explicitly returns in a monad that supports failure).

-}

totalizeFunction :: (NFData b, MonadThrow m) => (a -> b) -> (a -> m b)
totalizeFunction f = g
 where g x = spoonWith defaultPartialityHandlers (f x)

--------------------------------------------------

{-| handles the following exceptions:

* 'ArithException'
* 'ArrayException'
* 'ErrorCall'
* 'PatternMatchFail'
* 'SomeException': \[TODO rm]

-}

defaultPartialityHandlers :: (MonadThrow m) => [Handler (m a)]
defaultPartialityHandlers =
    [ Handler $ \(e :: AsyncException)   -> throwIO e -- TODO I hope they are tried in order
    , Handler $ \(e :: ArithException)   -> return (throwM e)
    , Handler $ \(e :: ArrayException)   -> return (throwM e)
    , Handler $ \(e :: ErrorCall)        -> return (throwM e)
    , Handler $ \(e :: PatternMatchFail) -> return (throwM e)
    , Handler $ \(e :: SomeException)    -> return (throwM e) -- TODO is catchall okay? why is this here?
    ]

{-# INLINEABLE defaultPartialityHandlers #-}

--------------------------------------------------

{-| Evaluate a value to normal form and 'throwM' any exceptions are thrown
during evaluation. For any error-free value, @spoon = Just@.

(taken from the
<https://hackage.haskell.org/package/spoon-0.3.1/docs/Control-Spoon.html spoon>
package.)

-}

spoonWith :: (NFData a, MonadThrow m) => [Handler (m a)] -> a -> m a
spoonWith handlers a = unsafePerformIO $ do
 (a `deepseq` (return `fmap` return a)) `catches` handlers

{-# INLINEABLE spoonWith #-}

--------------------------------------------------

{- | the eliminator as a function and the introducer as a string

helper for declaring Show instances of datatypes without
visible constructors (like @Map@ which is shown as a list).

-}

showsPrecWith :: (Show b) => String -> (a -> b) -> Int -> a -> ShowS
showsPrecWith stringFrom functionInto p x = showParen (p >= 11) $
  showString stringFrom . showString " " . shows (functionInto x)
-- showsPrecWith :: (Show a, Show b) => Name -> (a -> b) -> Int -> a -> ShowS
-- showsPrecWith nameFrom functionInto p x = showParen (p > 10) $
--   showString (nameBase nameFrom) . showString " " . shows (functionInto x)

--------------------------------------------------
--------------------------------------------------

{-| The cross-product of two lists.

>>> crossOrderingBoolean = crossProduct [LT,EQ,GT] [False,True]
>>> length crossOrderingBoolean
3
>>> length (Prelude.head crossOrderingBoolean)
2
>>> import Enumerate.Function.Extra (printMappings)
>>> printMappings crossOrderingBoolean
<BLANKLINE>
(LT,False)
(LT,True)
<BLANKLINE>
(EQ,False)
(EQ,True)
<BLANKLINE>
(GT,False)
(GT,True)

(with 'printMappings' defined internally).

The length of the outer list is the size of the first set, and
the length of the inner list is the size of the second set.

-}

crossProduct :: [a] -> [b] -> [[(a,b)]]
crossProduct [] _ = []
crossProduct (aValue:theDomain) theCodomain =
 fmap (aValue,) theCodomain : crossProduct theDomain theCodomain

--------------------------------------------------
--------------------------------------------------
-- for doctests:

{-| convert a power set to an isomorphic matrix, sorting the entries.

(for @doctest@)

-}

powerset2matrix :: Set (Set a) -> [[a]]
powerset2matrix = (List.sortBy (Ord.comparing length) . fmap Set.toList . Set.toList)

--------------------------------------------------

{-| (for @doctest@)
-}

printMappings :: (Show a) => [[a]] -> IO ()
printMappings mappings = traverse_ (\mapping -> (putStrLn"") >> (traverse print) mapping) mappings >> return()

--------------------------------------------------
--------------------------------------------------