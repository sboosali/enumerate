{-# LANGUAGE RankNTypes, LambdaCase, ScopedTypeVariables #-}
module Enumerate.Function.Extra
 ( module Enumerate.Function.Extra

 , module Control.DeepSeq
 , module Data.Semigroup

 , module GHC.Generics
 , module Data.Data
 , module Control.Arrow

 , module Data.Function
 , module Data.List
 , module Data.Foldable
 ) where

import Data.Semigroup (Semigroup)
import Control.DeepSeq (NFData(..), deepseq)
import Control.Monad.Catch (MonadThrow(..), SomeException(..))

import GHC.Generics (Generic)
import Data.Data (Data)
import Control.Arrow ((>>>))
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (catches, throwIO, Handler(..), AsyncException, ArithException, ArrayException, ErrorCall, PatternMatchFail)

import Data.Function ((&))
import Data.List (intercalate)
import Data.Foldable (traverse_)


nothing :: (Monad m) => m ()
nothing = return ()

maybe2bool :: Maybe a -> Bool
maybe2bool = maybe False (const True)

either2maybe :: Either e a -> Maybe a
either2maybe = either (const Nothing) Just

either2bool :: Either e a -> Bool
either2bool = either (const False) (const True)

{-| @failed = 'throwM' . 'userError'@

-}
failed ::  (MonadThrow m) => String -> m a
failed = throwM . userError

-- | generalize a function that fails with @Nothing@.
maybe2throw :: (a -> Maybe b) -> (forall m. MonadThrow m => a -> m b)
maybe2throw f = f >>> \case
 Nothing -> failed "Nothing"
 Just x  -> return x

-- | generalize a function that fails with @[]@.
list2throw :: (a -> [b]) -> (forall m. MonadThrow m => a -> m b)
list2throw f = f >>> \case
 []    -> failed "[]"

 (x:_) -> return x

-- | generalize a function that fails with @Left@.
either2throw :: (a -> Either SomeException b) -> (forall m. MonadThrow m => a -> m b)
either2throw f = f >>> \case
 Left  e -> throwM e
 Right x -> return x

{-| specialization -}
throw2maybe :: (forall m. MonadThrow m => a -> m b) -> (a -> Maybe b)
throw2maybe = id

{-| specialization -}
throw2either :: (forall m. MonadThrow m => a -> m b) -> (a -> Either SomeException b)
throw2either = id

{-| specialization -}
throw2list :: (forall m. MonadThrow m => a -> m b) -> (a -> [b])
throw2list = id

{-| makes an *unsafely*-partial function
(i.e. a function that throws exceptions or
that has inexhaustive pattern matching)
into a *safely*-partial function
(i.e. that explicitly returns in a monad that supports failure).

-}
totalizeFunction :: (NFData b, MonadThrow m) => (a -> b) -> (a -> m b)
totalizeFunction f = g
 where g x = spoonWith defaultPartialityHandlers (f x)

{-| handles the following exceptions:

* 'ArithException'
* 'ArrayException'
* 'ErrorCall'
* 'PatternMatchFail'

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

{- | the eliminator as a function and the introducer as a string

helper for declaring Show instances of datatypes without
visible constructors (like @Map@ which is shown as a list).

-}
showsPrecWith :: (Show b) => String -> (a -> b) -> Int -> a -> ShowS
showsPrecWith stringFrom functionInto p x = showParen (p > 10) $
  showString stringFrom . showString " " . shows (functionInto x)
-- showsPrecWith :: (Show a, Show b) => Name -> (a -> b) -> Int -> a -> ShowS
-- showsPrecWith nameFrom functionInto p x = showParen (p > 10) $
--   showString (nameBase nameFrom) . showString " " . shows (functionInto x)
