{-# LANGUAGE RankNTypes, LambdaCase, ScopedTypeVariables #-}
module Data.Enumerate.Extra where

import Control.Monad.Catch (MonadThrow(..), SomeException(..))
import Control.DeepSeq (NFData(..), deepseq) 

-- import Language.Haskell.TH.Syntax (Name,nameBase)
import Control.Arrow ((&&&), (>>>))
import System.IO.Unsafe (unsafePerformIO) 
import Control.Exception (catches, throwIO, Handler(..), AsyncException, ArithException, ArrayException, ErrorCall, PatternMatchFail)
import Data.Foldable  (traverse_)
import Numeric.Natural 


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

{-| makes an *unsafely*-partial function (i.e. a function that throws exceptions or that has inexhaustive pattern matching) into a *safely*-partial function (i.e. that explicitly returns in a monad that supports failure).


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
    , Handler $ \(e :: SomeException)    -> return (throwM e)
    ]
{-# INLINEABLE defaultPartialityHandlers #-}

{-| Evaluate a value to normal form and 'throwM' any exceptions are thrown during evaluation. For any error-free value, @spoon = Just@.

taken from the <https://hackage.haskell.org/package/spoon-0.3.1/docs/Control-Spoon.html spoon> package.  

-}
spoonWith :: (NFData a, MonadThrow m) => [Handler (m a)] -> a -> m a 
spoonWith handlers a = unsafePerformIO $ do 
 deepseq a (return `fmap` return a) `catches` handlers 
{-# INLINEABLE spoonWith #-}

{- | the eliminator as a function and the introducer as a string

helper for declaring Show instances of datatypes without visible constructors (like @Map@
which is shown as an list).

-}

showsPrecWith :: (Show a, Show b) => String -> (a -> b) -> Int -> a -> ShowS
showsPrecWith stringFrom functionInto p x = showParen (p > 10) $
  showString stringFrom . showString " " . shows (functionInto x)

-- showsPrecWith :: (Show a, Show b) => Name -> (a -> b) -> Int -> a -> ShowS
-- showsPrecWith nameFrom functionInto p x = showParen (p > 10) $
--   showString (nameBase nameFrom) . showString " " . shows (functionInto x)

int2natural :: Int -> Natural 
int2natural = fromInteger . toInteger

