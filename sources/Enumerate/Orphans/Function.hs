{-# LANGUAGE TypeFamilies, ExplicitNamespaces, DataKinds, UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-| orphan instances, of 'Enumerate'\/'Eq'\/'Show', for functions:

* @instance (Enumerable a, Enumerable b, Ord a,  Ord b)  => Enumerable (a -> b)@
* @instance (Enumerable a,               Eq b)           => Eq         (a -> b)@
* @instance (Enumerable a,               Show a, Show b) => Show       (a -> b)@

see:

* 'functionEnumerated', 'functionCardinality'
* 'extensionallyEqual', 'extensionallyUnequal'
* 'functionShowsPrec'

(that are included for completeness, but not exported by default
(i.e. by "Enumerate").
you probably want build-time instance-resolution errors,
rather than possible runtime non-termination).


@-- doctest@

>>> :set -XLambdaCase

-}
module Enumerate.Orphans.Function where
import Enumerate.Types
import Enumerate.Map


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
 enumerated  = functionEnumerated
 cardinality = functionCardinality

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
 (==) = extensionallyEqual
 (/=) = extensionallyUnequal

{-|

-- >>> not
-- unsafeFromList [(False,True),(True,False)]

because functions are curried, the instance is recursive,
and it works on functions of any arity:

-- >>> (&&)
-- unsafeFromList [(False,unsafeFromList [(False,False),(True,False)]),(True,unsafeFromList [(False,False),(True,True)])]

-}
instance (Enumerable a, Show a, Show b) => Show (a -> b) where
 showsPrec = functionShowsPrec
