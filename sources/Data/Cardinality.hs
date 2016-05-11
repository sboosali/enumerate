{-# LANGUAGE TypeFamilies, ExplicitNamespaces, TypeOperators, FlexibleInstances #-}
{-# LANGUAGE DataKinds, UndecidableInstances, ConstraintKinds #-}

module Data.Cardinality where

import           GHC.Generics
import Data.Vinyl (Rec)
import           Data.Proxy (Proxy)
import           Data.Void (Void)
import           Data.Word (Word8, Word16)
import           Data.Int (Int8, Int16)
import Data.Set (Set)
import Numeric.Natural (Natural)
import GHC.TypeLits (Nat, KnownNat, type (+), type (*), type (^), type (<=?))

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

{-|

(the 'Generic' constaint is necessary, because "@DefaultSignatures@"
for type family instances don't exist or make sense. But that's fine,
since most types should derive it anyway).

-}
-- class (Generic a) => Finite a where
--   type Cardinality a :: Nat
--   type Cardinality = GCardinality (Rep a)

class Finite a where
  type Cardinality a :: Nat

-- base types. TODO any more?
instance Finite Void
instance Finite ()
instance Finite Bool
instance Finite Ordering

instance Finite (Proxy a) where
 type Cardinality (Proxy a) = 1

instance Finite Int8 where
 type Cardinality Int8 = 256 -- ^ 2^8

instance Finite Word8 where
 type Cardinality Word8 = 256 -- 2^8

instance Finite Int16 where
  type Cardinality Int16 = 65536 -- 2^16

instance Finite Word16 where
 type Cardinality Word16 = 65536 -- 2^16

instance Finite Char where
 type Cardinality Char = 1114112

instance Finite (Maybe a) where
 type Cardinality (Maybe a) = 1 + (Cardinality a)

instance Finite (Either a b) where
 type Cardinality (Either a b) = (Cardinality a) + (Cardinality b)

{-| the cardinality is a product of cardinalities. -}
instance Finite (Rec f (a ': as)) where
 type Cardinality (Rec f (a ': as)) = (Cardinality (f a)) * (Cardinality (Rec f as))

instance Finite (Rec f '[]) where
 type Cardinality (Rec f '[]) = 1

{-
class Finite (Mod i n) where
 type Cardinality (Mod i n) = n
-}

instance (Finite a, Finite b) => Finite (a, b)

-- | 3
instance (Finite a, Finite b, Finite c) => Finite (a, b, c)
-- | 4
instance (Finite a, Finite b, Finite c, Finite d) => Finite (a, b, c, d)
-- | 5
instance (Finite a, Finite b, Finite c, Finite d, Finite e) => Finite (a, b, c, d, e)
-- | 6
instance (Finite a, Finite b, Finite c, Finite d, Finite e, Finite f) => Finite (a, b, c, d, e, f)
-- | 7
instance (Finite a, Finite b, Finite c, Finite d, Finite e, Finite f, Finite g) => Finite (a, b, c, d, e, f, g)

-- |
instance Finite (Set a) where
 type Cardinality (Set a) = 2 ^ (Cardinality a)

--------------------------------------------------------------------------------
--
-- type family GCardinality f :: Nat
--
-- type instance GCardinality (V1) = 0
--
-- type instance GCardinality (U1) = 1
--
-- type instance GCardinality (K1 i a) = Cardinality a
--
-- type instance GCardinality (f :+: g) = (GCardinality f) + (GCardinality g)
--
-- type instance GCardinality (f :*: g) = (GCardinality f) * (GCardinality g)
--
-- type instance GCardinality (M1 i t f) = GCardinality f
--
--------------------------------------------------------------------------------
--
-- {-| typechecks only when the constraint is satisifed.
--
-- a constaint.
--
-- -}
-- type CardinalityWithin n a = IsCardinalityWithin n a ~ True
--
-- {-|
--
-- >>> type CardinalityWithinAMillion = CardinalityWithin 1000000
-- >>> :kind! CardinalityWithinAMillion Bool
-- True
-- >>> :kind! CardinalityWithinAMillion Char
-- False
--
-- a predicate, inclusive.
--
-- -}
-- type IsCardinalityWithin n a = Cardinality a <=? n
--
-- -- {-| enumerate only when the cardinality is small enough.
-- --
-- -- >>> enumerateWithin 2 :: Either Natural [Bool]
-- -- Left 2
-- --
-- -- >>> enumerateWithin 100 :: Either Natural [Bool]
-- -- Right [False,True]
-- --
-- -- useful when you've established that traversing a list below some length
-- -- and consuming its values is reasonable for your application.
-- -- e.g. after benchmarking, you think you can process a billion entries within a minute.
-- --
-- -- -}
-- -- enumerateWithin :: forall a. (Enumerable a) => Natural -> Either Natural [a] --TODO move
-- -- enumerateWithin maxSize = if theSize < maxSize
-- --   then Right enumerated
-- --   else Left theSize
-- --  where
-- --  theSize = cardinality (Proxy :: Proxy a)
