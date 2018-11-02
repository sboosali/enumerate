{-# LANGUAGE TypeFamilies, ExplicitNamespaces, FlexibleInstances #-}
{-# LANGUAGE DataKinds, UndecidableInstances, ConstraintKinds, KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

{-# LANGUAGE TypeOperators #-}

--------------------------------------------------
--------------------------------------------------

{-| The cardinality of a finite type.

This cardinality is known /statically/ and at the /type-level/.

-}

module Enumerate.Cardinality where

--------------------------------------------------
--------------------------------------------------

import Enumerate.Extra
--import Enumerate.Compatibility (MultiplyNats)

--------------------------------------------------
-- Imports ---------------------------------------
--------------------------------------------------

import "vinyl" Data.Vinyl (Rec)

--------------------------------------------------

-- import Data.Set (Set)

--------------------------------------------------

-- import           Data.Proxy (Proxy)
import           "base" Data.Void (Void)
import           "base" Data.Word (Word8, Word16)
import           "base" Data.Int (Int8, Int16)

-- import Numeric.Natural (Natural)
-- import           Data.Proxy 

--------------------------------------------------

import           "base" GHC.Generics
import qualified "base" GHC.TypeLits as GHC
import           "base" GHC.TypeLits (Nat, KnownNat, natVal, type (+), type (^), type (<=?))

--------------------------------------------------

-- alternatives:
-- class Finite a where
-- type GenericCardinality a = GCardinality (Rep a)
-- class Cardinality a n
-- class Finite a where  type Cardinality a :: Nat
  {- needs DefaultTypeInstances,
  or we have to pick between deriving instances (the user should)
  and manually providing them (the author should, for base types like Char,
  because their Generic rep is huge and slows down the compiler to a stop)
 -}
 -- class GFinite a where
-- default type (Generic a) => Cardinality a = GCardinality (Rep a)
-- type instance {-# OVERLAPS #-} (Generic a) => Cardinality a = GCardinality (Rep a)

--------------------------------------------------
-- Instances -------------------------------------
--------------------------------------------------

{-| Typeclass for types that are finite, i.e. have bounded size.

The 'Cardinality' associated @type family@ is the type-level
analogue to the 'cardinality' method.

= Laws

[Consistency of Cardinality] @'cardinality' == 'reifyCardinality'@.
i.e. the value-level (a 'Natural') matches the type-level (a 'Nat')

= Examples

>>> reifyCardinality ([] :: [Bool])
2

-}

class Finite a where
  type Cardinality a :: Nat
  type Cardinality a = GCardinality (Rep a)

--------------------------------------------------
-- Instances -------------------------------------
--------------------------------------------------

-- base types. TODO any more?

-- | @0@
instance Finite Void
-- | @1@
instance Finite ()
-- | @2@
instance Finite Bool
-- | @3@
instance Finite Ordering

instance Finite (Proxy a) where
 type Cardinality (Proxy a) = 1

--------------------------------------------------

-- | @2^8@
instance Finite Int8 where
 type Cardinality Int8 = 256

-- | @2^8@
instance Finite Word8 where
 type Cardinality Word8 = 256

-- | @2^16@
instance Finite Int16 where
  type Cardinality Int16 = 65536

-- | @2^16@
instance Finite Word16 where
 type Cardinality Word16 = 65536

-- | @1114112@
instance Finite Char where
 type Cardinality Char = 1114112

--------------------------------------------------

-- | @1 + a@
instance (Finite a) => Finite (Maybe a) where
 type Cardinality (Maybe a) = 1 + (Cardinality a)

-- | @a + b@
instance (Finite a, Finite b) => Finite (Either a b) where
 type Cardinality (Either a b) = (Cardinality a) + (Cardinality b)

--------------------------------------------------

{-| the cardinality is a product of cardinalities. -}
instance (Finite (f a), Finite (Rec f as)) => Finite (Rec f (a ': as)) where
 type Cardinality (Rec f (a ': as)) = (Cardinality (f a)) GHC.* (Cardinality (Rec f as))

 -- | @1@
instance Finite (Rec f '[]) where
 type Cardinality (Rec f '[]) = 1

--------------------------------------------------

{-
class Finite (Mod i n) where
 type Cardinality (Mod i n) = n
-}

--------------------------------------------------

-- | @a*b@
instance (Finite a, Finite b) => Finite (a, b)

-- | @a*b*c@
instance (Finite a, Finite b, Finite c) => Finite (a, b, c)
-- | @a*b*c*d@
instance (Finite a, Finite b, Finite c, Finite d) => Finite (a, b, c, d)
-- | @a*b*c*d*e@
instance (Finite a, Finite b, Finite c, Finite d, Finite e) => Finite (a, b, c, d, e)
-- | @a*b*c*d*e*f@
instance (Finite a, Finite b, Finite c, Finite d, Finite e, Finite f) => Finite (a, b, c, d, e, f)
-- | @a*b*c*d*e*f*g@
instance (Finite a, Finite b, Finite c, Finite d, Finite e, Finite f, Finite g) => Finite (a, b, c, d, e, f, g)

-- | @2^a@
instance (Finite a) => Finite (Set a) where
 type Cardinality (Set a) = 2 ^ (Cardinality a)

-- | @b^a@
instance (Finite a, Finite b) => Finite (a -> b) where
 type Cardinality (a -> b) = (Cardinality b) ^ (Cardinality a)

--------------------------------------------------------------------------------

type family GCardinality (f :: * -> *) :: Nat
-- NOTE Use « * », not « Type », for backwards-compatibility with GHC-7.10.

type instance GCardinality (V1) = 0

type instance GCardinality (U1) = 1

type instance GCardinality (K1 i a) = Cardinality a

type instance GCardinality (f :+: g) = (GCardinality f) + (GCardinality g)

type instance GCardinality (f :*: g) = (GCardinality f) GHC.* (GCardinality g)
-- HACK « GHC.* » parses correctly (i.e. as a TypeOperator)
--      both under GHC-8.6 (with -XStarIsType) and under GHC-7.10

type instance GCardinality (M1 i t f) = GCardinality f

--------------------------------------------------------------------------------

{-|

>>> reifyCardinality ([]::[Bool])
2

-}
reifyCardinality
 :: forall a proxy. (KnownNat (Cardinality a))
 => proxy a
 -> Natural
reifyCardinality _ = fromInteger (natVal (Proxy::Proxy (Cardinality a)))

--------------------------------------------------

{-| typechecks only when the constraint is satisifed.

a constaint.

-}

type CardinalityWithin n a = IsCardinalityWithin n a ~ True

--------------------------------------------------

{-|

a predicate, inclusive.

@
> type CardinalityWithinAMillion a = CardinalityWithin 1000000 a
> :kind! CardinalityWithinAMillion Bool
True
> :kind! CardinalityWithinAMillion Char
False
@

-}

type IsCardinalityWithin n a = Cardinality a <=? n

--------------------------------------------------

{-
>>> :set -XDataKinds
>>> :set -XConstraintKinds
>>> :set -XTypeFamilies
>>> type CardinalityWithinAMillion a = CardinalityWithin 1000000 a
>>> :kind! CardinalityWithinAMillion Bool
True
>>> :kind! CardinalityWithinAMillion Char
False
-}

-- {-| enumerate only when the cardinality is small enough.
--
-- >>> enumerateWithin 2 :: Either Natural [Bool]
-- Left 2
--
-- >>> enumerateWithin 100 :: Either Natural [Bool]
-- Right [False,True]
--
-- useful when you've established that traversing a list below some length
-- and consuming its values is reasonable for your application.
-- e.g. after benchmarking, you think you can process a billion entries within a minute.
--
-- -}
-- enumerateWithin :: forall a. (Enumerable a) => Natural -> Either Natural [a] --TODO move
-- enumerateWithin maxSize = if theSize < maxSize
--   then Right enumerated
--   else Left theSize
--  where
--  theSize = cardinality (Proxy :: Proxy a)

--------------------------------------------------
--------------------------------------------------