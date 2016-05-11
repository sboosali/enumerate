{-# LANGUAGE TupleSections, ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies, ExplicitNamespaces, DataKinds, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-| orphan instances, of 'Enumerate'\/'Eq'\/'Show', for functions.

(that are included for completeness, but not exported by default
(i.e. by "Enumerate").
you probably want build-time instance-resolution errors,
rather than possible runtime non-termination).


@-- doctest@

>>> :set -XLambdaCase

-}
module Enumerate.Function where
import Enumerate.Types
import Enumerate.Reify
import Enumerate.Map
import Enumerate.Extra

import           Data.Proxy
import qualified Data.Map as Map
-- import GHC.TypeLits (Nat, type (^))


{-| the exponential type.

the 'cardinality' is the cardinality of @b@ raised to the cardinality @a@, i.e. @|b|^|a|@.

warning: it grows very quickly.

might be useful for generating random functions on small types,
like to fuzz test type class laws.

the 'cardinality' call is efficient (depending on the efficiency of the base type's call).
you should be able to safely (WRT performance) call 'enumerateBelow',
unless the arithmetic itself becomes too expensive.

@
instance ('Enumerable' a, Enumerable b, 'Ord' a, Ord b) => Enumerable (a -> b) where
 enumerated = 'functionEnumerated'
@

-}
instance (Enumerable a, Enumerable b, Ord a, Ord b) => Enumerable (a -> b) where --TODO, no (oprhan) instance, just the standalone function/type-instance?
 -- -- type Cardinality (a -> b) = (Cardinality b) ^ (Cardinality a)
 enumerated    = functionEnumerated
 cardinality _ = cardinality (Proxy :: Proxy b) ^ cardinality (Proxy :: Proxy a)


{-| brute-force function extensionality.

warning: the size of the domain grows exponentially in the number of arguments.

>>> (\case LT -> False; EQ -> False; GT -> False) == const False
True
>>> (\case LT -> False; EQ -> False; GT -> False) == const True
False

because functions are curried, the instance is recursive,
and it works on functions of any arity:

> -- De Morgan's laws
>>> (\x y -> not (x && y)) == (\x y -> not x || not y)
True
>>> (\x y -> not (x || y)) == (\x y -> not x && not y)
True

-}
instance (Enumerable a, Eq b) => Eq (a -> b) where
 f == g = all ((==) <$> f <*> g) enumerated
 f /= g = any ((/=) <$> f <*> g) enumerated


{-|

-- >>> not
-- unsafeFromList [(False,True),(True,False)]

because functions are curried, the instance is recursive,
and it works on functions of any arity:

-- >>> (&&)
-- unsafeFromList [(False,unsafeFromList [(False,False),(True,False)]),(True,unsafeFromList [(False,False),(True,True)])]

-}
instance (Enumerable a, Show a, Show b) => Show (a -> b) where
 showsPrec = showsPrecWith "unsafeFromList" reifyFunction


{-| wraps 'Map.lookup'

>>> (unsafeFromList [(False,True),(True,False)]) False
True
>>> (unsafeFromList [(False,True),(True,False)]) True
False

-}
unsafeFromList :: (Ord a) => [(a,b)] -> (a -> b)
unsafeFromList l = unsafeToFunction (Map.fromList l)
{-# INLINABLE unsafeFromList #-}

{-| see 'mappingEnumeratedAt' -}
functionEnumerated :: (Enumerable a, Enumerable b, Ord a, Ord b) => [a -> b]
functionEnumerated = functions
 where
 functions = (unsafeToFunction . Map.fromList) <$> mappings
 mappings = mappingEnumeratedAt enumerated enumerated


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
