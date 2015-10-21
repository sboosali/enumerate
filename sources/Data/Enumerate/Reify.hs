{-# LANGUAGE RankNTypes, LambdaCase #-}
{-| see 'reifyFunctionAtM'.  

@-- doctest@

>>> :set +m

-}
module Data.Enumerate.Reify where
import Data.Enumerate.Types 
import Data.Enumerate.Extra 

import Control.Monad.Catch (MonadThrow(..), SomeException(..)) 
import Control.DeepSeq (NFData) 

import Control.Arrow ((&&&))


{- | reify a total function. 

@
>>> reifyFunction 'not'
[(False,True),(True,False)]
@

-} 
reifyFunction :: (Enumerable a) => (a -> b) -> [(a,b)]
reifyFunction f = reifyFunctionM (return . f)
{-# INLINABLE reifyFunction #-}

-- | reify a total function at any subset of the domain. 
reifyFunctionAt :: [a] -> (a -> b) -> [(a,b)]
reifyFunctionAt domain f = reifyFunctionAtM domain (return . f)
{-# INLINABLE reifyFunctionAt #-}

-- | reify a (safely-)partial function into a map (which is implicitly partial, where @Map.lookup@ is like @($)@.
reifyFunctionM :: (Enumerable a) => (forall m. MonadThrow m => a -> m b) -> [(a,b)]
reifyFunctionM = reifyFunctionAtM enumerated
{-# INLINABLE reifyFunctionM #-}

{- | reify a (safely-)partial function at any domain. 

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

>>> :{
let uppercasePartial :: (MonadThrow m) => Char -> m Char 
    uppercasePartial c = case c of
     'a' -> return 'A'
     'b' -> return 'B'
     'z' -> return 'Z'
     _   -> failed "uppercasePartial"
:}

@
>>> reifyFunctionAtM ['a'..'c'] uppercasePartial
[('a','A'),('b','B')] 
@

if your function doesn't fail under 'MonadThrow', see: 

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

-- | reify a (safely-)partial function that fails specifically under @Maybe@. 
reifyFunctionMaybeAt :: [a] -> (a -> Maybe b) -> [(a, b)]
reifyFunctionMaybeAt domain f = reifyFunctionAtM domain (maybe2throw f)
{-# INLINABLE reifyFunctionMaybeAt #-}

-- | reify a (safely-)partial function that fails specifically under @[]@. 
reifyFunctionListAt :: [a] -> (a -> [b]) -> [(a, b)]
reifyFunctionListAt domain f = reifyFunctionAtM domain (list2throw f)
{-# INLINABLE reifyFunctionListAt #-}

-- | reify a (safely-)partial function that fails specifically under @Either SomeException@. 
reifyFunctionEitherAt :: [a] -> (a -> Either SomeException b) -> [(a, b)]
reifyFunctionEitherAt domain f = reifyFunctionAtM domain (either2throw f)
{-# INLINABLE reifyFunctionEitherAt #-}

{-| reifies an *unsafely*-partial function (i.e. a function that throws exceptions or that has inexhaustive pattern matching).

forces the function to be strict.

@
>>> import Data.Ratio (Ratio) 
>>> fmap (1/) [0..3 :: Ratio Integer]
[*** Exception: Ratio has zero denominator
>>> let (1/) = reciprocal 
>>> reifyFunctionSpoonAt [0..3 :: Ratio Integer] reciprocal 
[(1 % 1,1 % 1),(2 % 1,1 % 2),(3 % 1,1 % 3)]
@

normal caveats from violating purity (via @unsafePerformIO@) and from catchalls (via @(e :: SomeExceptions -> _)@) apply.

-}
reifyFunctionSpoonAt :: (NFData b) => [a] -> (a -> b) -> [(a, b)]
reifyFunctionSpoonAt domain f = reifyFunctionMaybeAt domain (totalizeFunction f)

-- | reify a binary total function
reifyFunction2 :: (Enumerable a, Enumerable b) => (a -> b -> c) -> [(a,[(b,c)])]
reifyFunction2 f = reifyFunction2At enumerated enumerated f
{-# INLINABLE reifyFunction2 #-}

-- | reify a binary total function at some domain
reifyFunction2At :: [a] -> [b] -> (a -> b -> c) -> [(a,[(b,c)])]
reifyFunction2At as bs f = reifyFunction2AtM as bs (\x y -> pure (f x y))
{-# INLINABLE reifyFunction2At #-}

-- | reify a binary (safely-)partial function
reifyFunction2M :: (Enumerable a, Enumerable b) => (forall m. MonadThrow m => a -> b -> m c) -> [(a,[(b,c)])]
reifyFunction2M f = reifyFunction2AtM enumerated enumerated f
{-# INLINABLE reifyFunction2M #-}

-- | reify a binary (safely-)partial function at some domain 
reifyFunction2AtM :: [a] -> [b] -> (forall m. MonadThrow m => a -> b -> m c) -> [(a,[(b,c)])]
reifyFunction2AtM as bs f = reifyFunctionAt as (\a -> reifyFunctionAtM bs (f a))

