{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes, LambdaCase, TupleSections #-}
{-| converting between partial functions and maps.

@(for doctest)@

>>> :set +m
>>> :set -XLambdaCase
>>> :{
let uppercasePartial :: (MonadThrow m) => Char -> m Char  -- :: Partial Char Char
    uppercasePartial = \case
     'a' -> return 'A'
     'b' -> return 'B'
     'z' -> return 'Z'
     _   -> failed "uppercasePartial"
:}

a (safely-)partial function is isomorphic with a @Map@:

@
'fromFunctionM' . 'toFunctionM' = 'id'
'toFunctionM' . 'fromFunctionM' = 'id'
@

modulo the error thrown.

-}
module Enumerate.Function.Map where
import Enumerate.Types
import Enumerate.Function.Extra
import Enumerate.Function.Types
import Enumerate.Function.Reify
import Enumerate.Function.Invert

import Control.Monad.Catch (MonadThrow(..))

-- import GHC.TypeLits (Nat, type (^))
import qualified Data.Map as Map
import           Data.Map (Map)
import           Control.Exception(PatternMatchFail(..))
import           Data.Proxy
import           Numeric.Natural
import           Data.Maybe (fromJust)


{- | convert a map to a function, if the map is total.

>>> let (Just not_) = toFunction (Map.fromList [(False,True),(True,False)])
>>> not_ False
True

-}
toFunction :: (Enumerable a, Ord a) => Map a b -> Maybe (a -> b)
toFunction m = if isMapTotal m then Just f else Nothing
 where f = unsafeToFunction m -- the fromJust is safe when the map is total
{-# INLINABLE toFunction #-}

{- | convert a (safely-)partial function to a map.

lookup failures are 'throwM'n as a 'PatternMatchFail'.

>>> let idPartial = toFunctionM (Map.fromList [(True,True)])
>>> idPartial True
True

>>> idPartial False
*** Exception: toFunctionM

-}
toFunctionM :: (Enumerable a, Ord a) => Map a b -> (Partial a b)
toFunctionM m = f
 where
 f x = maybe (throwM (PatternMatchFail "toFunctionM")) return (Map.lookup x m)
{-# INLINABLE toFunctionM #-}

{-| wraps 'Map.lookup'

-}
unsafeToFunction :: (Ord a) => Map a b -> (a -> b)
unsafeToFunction m x = fromJust (Map.lookup x m)
{-# INLINABLE unsafeToFunction #-}


{-| refines the partial function, if total.

>>> :{
let myNotM :: Monad m => Bool -> m Bool
    myNotM False = return True
    myNotM True  = return False
:}
>>> let (Just myNot) = isTotalM myNotM
>>> myNot False
True

-}
isTotalM :: (Enumerable a, Ord a) => (Partial a b) -> Maybe (a -> b)
isTotalM f = (toFunction) (fromFunctionM f)

--------------------------------------------------------------------------------
{-| wraps 'Map.lookup'

>>> (unsafeFromList [(False,True),(True,False)]) False
True
>>> (unsafeFromList [(False,True),(True,False)]) True
False

-}
unsafeFromList
 :: (Ord a)
 => [(a,b)]
 -> (a -> b)
unsafeFromList
 = unsafeToFunction . Map.fromList
{-# INLINABLE unsafeFromList #-}

{-| see 'mappingEnumeratedAt' -}
functionEnumerated
 :: (Enumerable a, Enumerable b, Ord a, Ord b)
 => [a -> b]
functionEnumerated = functions
 where
 functions = (unsafeToFunction . Map.fromList) <$> mappings
 mappings = mappingEnumeratedAt enumerated enumerated

-- | @|b| ^ |a|@
functionCardinality
 :: forall a b proxy. (Enumerable a, Enumerable b)
 => proxy (a -> b)
 -> Natural
functionCardinality _
 = cardinality (Proxy :: Proxy b) ^ cardinality (Proxy :: Proxy a)
{-# INLINABLE functionCardinality #-}

-- | are all pairs of outputs the same for the same input? (short-ciruits).
extensionallyEqual
 :: (Enumerable a, Eq b)
 => (a -> b)
 -> (a -> b)
 -> Bool
extensionallyEqual f g
 = all ((==) <$> f <*> g) enumerated
{-# INLINABLE extensionallyEqual #-}

-- | is any pair of outputs different for the same input? (short-ciruits).
extensionallyUnequal
 :: (Enumerable a, Eq b)
 => (a -> b)
 -> (a -> b)
 -> Bool
extensionallyUnequal f g
 = any ((/=) <$> f <*> g) enumerated
{-# INLINABLE extensionallyUnequal #-}

-- | show all inputs and their outputs, as @unsafeFromList [...]@.
functionShowsPrec
 :: (Enumerable a, Show a, Show b)
 => Int
 -> (a -> b)
 -> ShowS
functionShowsPrec
 = showsPrecWith "unsafeFromList" reifyFunction
{-# INLINABLE functionShowsPrec #-}

-- | show all inputs and their outputs, as @\case ...@.
displayFunction
  :: (Enumerable a, Show a, Show b)
  => (a -> b)
  -> String
displayFunction
    = reifyFunction
  >>> fmap displayCase
  >>> ("\\case":)
  >>> intercalate "\n"
 where
 displayCase (x,y) = intercalate " " ["", show x, "->", show y]

-- displayPartialFunction
--  :: (Enumerable a, Show a, Show b)
--  => (Partial a b)
--  -> String

displayInjective
 :: (Enumerable a, Ord a, Ord b, Show a, Show b)
 => (a -> b)
 -> Maybe String
displayInjective f = case isInjective f of
  Nothing -> Nothing
  Just{}  -> Just (go f)
  where
  go   = reifyFunction
     >>> fmap displayCase
     >>> (["\\case"]++)
     >>> (++[" _ <- Nothing"])
     >>> intercalate "\n"
  displayCase (x,y) = intercalate " " ["", show y, "<-", show (Just x)]

  -- displayInjective f = go <$> isInjective f
  --
  --   where
  --   go   = reifyFunction
  --      >>> fmap displayCase
  --      >>> ("\\case":)
  --      >>> intercalate "\n"
  --   displayCase = \case
  --    (y, Nothing) ->
  --    (y, Just x)  -> intercalate " " ["", show y, " <- ", show x]

{-| @[(a,b)]@ is a mapping, @[[(a,b)]]@ is a list of mappings.

>>> let orderingPredicates = mappingEnumeratedAt [LT,EQ,GT] [False,True]
>>> print $ length orderingPredicates
8
>>> printMappings $ orderingPredicates
<BLANKLINE>
(LT,False)
(EQ,False)
(GT,False)
<BLANKLINE>
(LT,False)
(EQ,False)
(GT,True)
<BLANKLINE>
(LT,False)
(EQ,True)
(GT,False)
<BLANKLINE>
(LT,False)
(EQ,True)
(GT,True)
<BLANKLINE>
(LT,True)
(EQ,False)
(GT,False)
<BLANKLINE>
(LT,True)
(EQ,False)
(GT,True)
<BLANKLINE>
(LT,True)
(EQ,True)
(GT,False)
<BLANKLINE>
(LT,True)
(EQ,True)
(GT,True)

where the (total) mapping:

@
(LT,False)
(EQ,False)
(GT,True)
@

is equivalent to the function:

@
\\case
 LT -> False
 EQ -> False
 GT -> True
@

-}
mappingEnumeratedAt :: [a] -> [b] -> [[(a,b)]]           -- TODO diagonalize? performance?
mappingEnumeratedAt as bs = go (crossProduct as bs)
 where
 go [] = []
 go [somePairs] = do
  pair <- somePairs
  return$ [pair]
 go (somePairs:theProduct) = do
  pair <- somePairs
  theExponent <- go theProduct
  return$ pair : theExponent

{-|

>>> let crossOrderingBoolean = crossProduct [LT,EQ,GT] [False,True]
>>> printMappings $ crossOrderingBoolean
<BLANKLINE>
(LT,False)
(LT,True)
<BLANKLINE>
(EQ,False)
(EQ,True)
<BLANKLINE>
(GT,False)
(GT,True)

the length of the outer list is the size of the first set, and
the length of the inner list is the size of the second set.

>>> print $ length crossOrderingBoolean
3
>>> print $ length (head crossOrderingBoolean)
2

-}
crossProduct :: [a] -> [b] -> [[(a,b)]]
crossProduct [] _ = []
crossProduct (aValue:theDomain) theCodomain =
 fmap (aValue,) theCodomain : crossProduct theDomain theCodomain
