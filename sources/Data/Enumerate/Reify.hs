{-# LANGUAGE RankNTypes, LambdaCase #-}
module Data.Enumerate.Reify where
import Data.Enumerate.Types 
import Data.Enumerate.Extra 

import Control.Monad.Catch

import Control.Arrow ((&&&))


-- | reify a total function
reifyFunction :: (Enumerable a) => (a -> b) -> [(a,b)]
reifyFunction f = reifyFunctionM (return . f)
{-# INLINABLE reifyFunction #-}

-- | reify a total function at any subset of the domain. 
reifyFunctionAt :: [a] -> (a -> b) -> [(a,b)]
reifyFunctionAt domain f = reifyFunctionAtM domain (return . f)
{-# INLINABLE reifyFunctionAt #-}

-- | reify a partial function into a map (which is implicitly partial, where @Map.lookup@ is like @($)@.
reifyFunctionM :: (Enumerable a) => (forall m. MonadThrow m => a -> m b) -> [(a,b)]
reifyFunctionM= reifyFunctionAtM enumerated
{-# INLINABLE reifyFunctionM #-}

{- | reify a partial function at any domain. 
use when your domain isn't 'Enumerable'. 
the most general function in this module.

>>> :set +m
>>> :{
let uppercasePartial :: Char -> Possibly Char 
    uppercasePartial c = case c of
     'a' -> return 'A'
     'b' -> return 'B'
     'z' -> return 'Z'
     _   -> failed "uppercasePartial"
:}

>>> reifyFunctionAtM ['a'..'c'] uppercasePartial
[('a','A'),('b','B')] 

@ 

if your function doesn't fail under @MonadThrow@, see: 

* 'reifyFunctionAtMaybe'
* 'reifyFunctionAtList'
* 'reifyFunctionAtEither'

-}
reifyFunctionAtM :: [a] -> (forall m. MonadThrow m => a -> m b) -> [(a,b)]
-- reifyFunctionAtM :: (MonadThrow m) => [a] -> (a -> m b) -> m (Map a b)
reifyFunctionAtM domain f 
 = concatMap (bitraverse pure id)
 . fmap (id &&& f)
 $ domain
 where
 bitraverse f g (x,y) = (,) <$> f x <*> g y  -- avoid bifunctors dependency

-- | @reifyPredicateAt = 'flip' 'filter'@
reifyPredicateAt :: [a] -> (a -> Bool) -> [a]
reifyPredicateAt = flip filter
-- reifyPredicateAtM domain p = map fst (reifyFunctionAtM domain f)
--  where
--  f x = if p x then return x else throwM (ErrorCall "False")

-- MonadThrow Maybe	 
-- (e ~ SomeException) => MonadThrow (Either e)
-- MonadThrow []	 

-- | reify a partial function that fails specifically under @Maybe@. 
reifyFunctionAtMaybe :: [a] -> (a -> Maybe b) -> [(a, b)]
reifyFunctionAtMaybe domain f = reifyFunctionAtM domain (maybe2throw f)
{-# INLINABLE reifyFunctionAtMaybe #-}

-- | reify a partial function that fails specifically under @[]@. 
reifyFunctionAtList :: [a] -> (a -> [b]) -> [(a, b)]
reifyFunctionAtList domain f = reifyFunctionAtM domain (list2throw f)
{-# INLINABLE reifyFunctionAtList #-}

-- | reify a partial function that fails specifically under @Either SomeException@. 
reifyFunctionAtEither :: [a] -> (a -> Either SomeException b) -> [(a, b)]
reifyFunctionAtEither  domain f = reifyFunctionAtM domain (either2throw f)
{-# INLINABLE reifyFunctionAtEither #-}

-- forces function to be strict 
-- reifyFunctionSpoon 

-- | reify a binary total function
reifyFunction2 :: (Enumerable a, Enumerable b) => (a -> b -> c) -> [(a,[(b,c)])]
reifyFunction2 f = reifyFunction2At enumerated enumerated f
{-# INLINABLE reifyFunction2 #-}

-- | reify a binary total function at some domain
reifyFunction2At :: [a] -> [b] -> (a -> b -> c) -> [(a,[(b,c)])]
reifyFunction2At as bs f = reifyFunction2AtM as bs (\x y -> pure (f x y))
{-# INLINABLE reifyFunction2At #-}

-- | reify a binary partial function
reifyFunction2M :: (Enumerable a, Enumerable b) => (forall m. MonadThrow m => a -> b -> m c) -> [(a,[(b,c)])]
reifyFunction2M f = reifyFunction2AtM enumerated enumerated f
{-# INLINABLE reifyFunction2M #-}

-- | reify a binary partial function at some domain 
reifyFunction2AtM :: [a] -> [b] -> (forall m. MonadThrow m => a -> b -> m c) -> [(a,[(b,c)])]
reifyFunction2AtM as bs f = reifyFunctionAt as (\a -> reifyFunctionAtM bs (f a))
