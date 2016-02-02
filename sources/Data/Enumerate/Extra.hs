{-# LANGUAGE RankNTypes, LambdaCase, ScopedTypeVariables #-}
{-| 

-}
module Data.Enumerate.Extra where

import Control.Monad.Catch (MonadThrow(..), SomeException(..))
import Control.DeepSeq (NFData(..), deepseq) 

-- import Language.Haskell.TH.Syntax (Name,nameBase)
import Control.Arrow ((&&&), (>>>))
import System.IO.Unsafe (unsafePerformIO) 
import Control.Exception (catches, throwIO, Handler(..), AsyncException, ArithException, ArrayException, ErrorCall, PatternMatchFail)
import Data.Foldable  (traverse_)
import Numeric.Natural 
import qualified Data.Set as Set
import Data.Set (Set) 
import qualified Data.List as List 
import qualified Data.Ord as Ord


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

{-| the power set of a set of values. 

>>> (powerset2matrix . powerSet . Set.fromList) [1..3]
[[],[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]]

-}
powerSet :: (Ord a) => Set a -> Set (Set a) 
powerSet values = Set.singleton values `Set.union` _Set_bind powerSet (dropEach values) 
 where 
 _Set_bind :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b 
 _Set_bind f = _Set_join . Set.map f 
 _Set_join :: (Ord a) => Set (Set a) -> Set a
 _Set_join = Set.unions . Set.toList 

{-| >>> (powerset2matrix . dropEach . Set.fromList) [1..3]
[[1,2],[1,3],[2,3]]

-}
dropEach :: (Ord a) => Set a -> Set (Set a) 
dropEach values = Set.map dropOne values 
 where
 dropOne value = Set.delete value values 

{-| convert a power set to an isomorphic matrix, sorting the entries. 

(for doctest) 

-}
powerset2matrix :: Set (Set a) -> [[a]] 
powerset2matrix = (List.sortBy (Ord.comparing length) . fmap Set.toList . Set.toList)

