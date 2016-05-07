{-# LANGUAGE RankNTypes, ScopedTypeVariables, DefaultSignatures, TypeOperators, FlexibleInstances, FlexibleContexts, LambdaCase, DataKinds  #-}
{- | see the 'Enumerable' class for documentation.

see "Data.Enumerate.Example" for examples.

can also help automatically derive @<https://hackage.haskell.org/package/QuickCheck/docs/Test-QuickCheck-Arbitrary.html QuickCheck>@ instances:

@
newtype SmallNatural = ...
instance Enumerable SmallNatural where ...
newtype SmallString = ...
instance Enumerable SmallString where  ...
data T = C0 | C1 () Bool SmallNatural SmallString | C2 ...
instance Arbitrary T where arbitrary = elements 'enumerated'
@


background on @Generics@:

* <https://hackage.haskell.org/package/base-4.8.1.0/docs/GHC-Generics.html GHC.Generics>


also provides instances for:

* sets

* modular integers

* vinyl records


related packages:

* <http://hackage.haskell.org/package/emgm-0.4/docs/Generics-EMGM-Functions-Enum.html emgm>.  allows infinite lists (by convention). too heavyweight.

* <http://hackage.haskell.org/package/enumerable enumerable>. no @Generic@ instance.

* <https://hackage.haskell.org/package/testing-feat-0.4.0.2/docs/Test-Feat-Class.html#t:Enumerable testing-feat>. too heavyweight (testing framework).

* <https://hackage.haskell.org/package/smallcheck smallcheck> too heavyweight (testing framework). Series enumerates up to some depth and can enumerated infinitely-inhabited types.

* https://hackage.haskell.org/package/quickcheck quickcheck> too heavyweight (testing framework, randomness unnecessary).

-}

module Data.Enumerate.Types where
import Data.Enumerate.Extra

--import Data.Modular
import Data.Vinyl (Rec(..))
import Control.Monad.Catch (MonadThrow(..))

import           GHC.Generics
import           Data.Proxy
import           Control.Arrow ((&&&))
import           Data.List (genericLength)
import           Data.Void (Void)
import           Data.Word (Word8, Word16)
import           Data.Int (Int8, Int16)
import qualified Data.Set as Set
import Data.Set (Set)
import System.Timeout
import Control.DeepSeq (NFData,force)
import GHC.TypeLits
import Numeric.Natural
import Data.Ix


{- | enumerate the set of all values in a (finitely enumerable) type. enumerates depth first.

generalizes 'Enum's to any finite/discrete type. an Enumerable is either:

* an Enum
* a product of Enumerables
* a sum of Enumerables

can be implemented automatically via its 'Generic' instance.

laws:

* consistent:

    * @'cardinality' = 'length' 'enumerated'@

    so you can index the 'enumerated' with a nonnegative index below the 'cardinality'.

* distinct:

    * @(Eq a) => 'nub' 'enumerated' == 'enumerated'@

* complete:

    * @x `'elem'` 'enumerated'@

* coincides with @Bounded@ @Enum@s:

    * @('Enum' a, 'Bounded' a) => 'enumerated' == 'boundedEnumerated'@

    * @('Enum' a) => 'enumerated' == 'enumEnumerated'@

(@Bounded@ constraint elided for convenience, but relevant.)

("inputs" a type, outputs a list of values).

-}
class Enumerable a where

 enumerated :: [a]

 default enumerated :: (Generic a, GEnumerable (Rep a)) => [a]
 enumerated = to <$> genumerated

 cardinality :: proxy a -> Natural
 cardinality _ = genericLength (enumerated :: [a])
 -- overrideable for performance, but don't lie!

 -- default cardinality :: (Generic a, GEnumerable (Rep a)) => proxy a -> Natural
 -- cardinality _ = gcardinality (Proxy :: Proxy (Rep a))
 -- TODO merge both methods into one that returns their pair

{-| a (safely-)partial function. i.e. a function that:

* fails only via the 'throwM' method of 'MonadThrow'
* succeeds only via the 'return' method of 'Monad'


-}
type Partial a b = (forall m. MonadThrow m => a -> m b)

-- | "Generic Enumerable", lifted to unary type constructors.
class GEnumerable f where
 genumerated :: [f x]
 gcardinality :: proxy f -> Natural

-- | empty list
instance GEnumerable (V1) where
 genumerated    = []
 gcardinality _ = 0
 {-# INLINE gcardinality #-}

-- | singleton list
instance GEnumerable (U1) where
 genumerated    = [U1]
 gcardinality _ = 1
 {-# INLINE gcardinality #-}

{-| call 'enumerated'

-}
instance (Enumerable a) => GEnumerable (K1 R a) where
 genumerated    = K1 <$> enumerated
 gcardinality _ = cardinality (Proxy :: Proxy a)
 {-# INLINE gcardinality #-}

-- | multiply lists with @concatMap@
instance (GEnumerable (f), GEnumerable (g)) => GEnumerable (f :*: g) where
 genumerated    = (:*:) <$> genumerated <*> genumerated
 gcardinality _ = gcardinality (Proxy :: Proxy (f)) * gcardinality (Proxy :: Proxy (g))
 {-# INLINE gcardinality #-}

-- | add lists with @(<>)@
instance (GEnumerable (f), GEnumerable (g)) => GEnumerable (f :+: g) where
 genumerated    = map L1 genumerated ++ map R1 genumerated
 gcardinality _ = gcardinality (Proxy :: Proxy (f)) + gcardinality (Proxy :: Proxy (g))
 {-# INLINE gcardinality #-}

-- | ignore selector metadata
instance (GEnumerable (f)) => GEnumerable (M1 S t f) where
 genumerated    = M1 <$> genumerated
 gcardinality _ = gcardinality (Proxy :: Proxy (f))
 {-# INLINE gcardinality #-}

-- | ignore constructor metadata
instance (GEnumerable (f)) => GEnumerable (M1 C t f) where
 genumerated    = M1 <$> genumerated
 gcardinality _ = gcardinality (Proxy :: Proxy (f))
 {-# INLINE gcardinality #-}

-- | ignore datatype metadata
instance (GEnumerable (f)) => GEnumerable (M1 D t f) where
 genumerated    = M1 <$> genumerated
 gcardinality _ = gcardinality (Proxy :: Proxy (f))
 {-# INLINE gcardinality #-}

{-| see "Data.Enumerate.Reify.getJectivityM"

-}
data Jectivity = Injective | Surjective | Bijective deriving (Show,Read,Eq,Ord,Enum,Bounded)

{-| wrap any @(Bounded a, Enum a)@ to be a @Enumerable@ via 'boundedEnumerated'.

(avoids @OverlappingInstances@).

-}
newtype WrappedBoundedEnum a = WrappedBoundedEnum { unwrapBoundedEnum :: a }

instance (Bounded a, Enum a) => Enumerable (WrappedBoundedEnum a) where
 enumerated    = WrappedBoundedEnum <$> boundedEnumerated
 cardinality _ = boundedCardinality (Proxy :: Proxy a)

-- base types
instance Enumerable Void
instance Enumerable ()
instance Enumerable Bool
instance Enumerable Ordering

{- |

@-- 'toInteger' prevents overflow@
>>> toInteger (maxBound::Int8) - toInteger (minBound::Int8)
255

-}
instance Enumerable Int8  where enumerated = boundedEnumerated; cardinality = boundedCardinality
instance Enumerable Word8 where enumerated = boundedEnumerated; cardinality = boundedCardinality
{- |

>>> toInteger (maxBound::Int16) - toInteger (minBound::Int16)
65535

-}
instance Enumerable Int16  where enumerated = boundedEnumerated; cardinality = boundedCardinality
instance Enumerable Word16 where enumerated = boundedEnumerated; cardinality = boundedCardinality
{- | there are only a million (1,114,112) characters.

>>> import Data.Char (ord,chr)  -- 'ord', 'chr'

>>> ord minBound
0

>>> ord maxBound
1114111

>>> length [chr 0 ..]
1114112

-}
instance Enumerable Char where enumerated = boundedEnumerated; cardinality = boundedCardinality

{-| the sum type.

the 'cardinality' is the sum of the cardinalities of @a@ and @b@.

-}
instance (Enumerable a, Enumerable b) => Enumerable (Either a b) where
 enumerated    = (Left <$> enumerated) ++ (Right <$> enumerated)
 cardinality _ = cardinality (Proxy :: Proxy a) + cardinality (Proxy :: Proxy b)
instance (Enumerable a) => Enumerable (Maybe a) where
 enumerated    = Nothing : (Just <$> enumerated)
 cardinality _ = 1 + cardinality (Proxy :: Proxy a)

{-| the product type.

the 'cardinality' is the product of the cardinalities of @a@ and @b@.

-}
instance (Enumerable a, Enumerable b) => Enumerable (a, b) where
 enumerated    = (,) <$> enumerated <*> enumerated
 cardinality _ = cardinality (Proxy :: Proxy a) * cardinality (Proxy :: Proxy b)

instance (Enumerable a, Enumerable b, Enumerable c) => Enumerable (a, b, c)
instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d) => Enumerable (a, b, c, d)
instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e) => Enumerable (a, b, c, d, e)
instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e, Enumerable f) => Enumerable (a, b, c, d, e, f)
instance (Enumerable a, Enumerable b, Enumerable c, Enumerable d, Enumerable e, Enumerable f, Enumerable g) => Enumerable (a, b, c, d, e, f, g)

{-| the cardinality is product of cardinalities. -}
instance (Enumerable (f a), Enumerable (Rec f as)) => Enumerable (Rec f (a ': as)) where
 enumerated =  (:&) <$> enumerated <*> enumerated
 cardinality _ = cardinality (Proxy :: Proxy (f a)) * cardinality (Proxy :: Proxy (Rec f as))

{-| the cardinality is 1. -}
instance Enumerable (Rec f '[]) where
 enumerated =  [RNil]
 cardinality _ = 1

{-|

the 'cardinality' is the cardinality of the 'powerSet' of @a@, i.e. @2^|a|@.
warning: it grows quickly. don't try to take the power set of 'Char'! or even 'Word8'.

the 'cardinality' call is efficient (depending on the efficiency of the base type's call).
you should be able to safely call 'enumerateBelow', unless the arithmetic itself becomes too large.


-}
instance (Enumerable a, Ord a) => Enumerable (Set a) where
 enumerated    = (Set.toList . powerSet . Set.fromList) enumerated
 cardinality _ = 2 ^ cardinality (Proxy :: Proxy a)

{-
-- | (from the @modular-arithmetic@ package)
instance (Integral i, Num i, KnownNat n) => Enumerable (Mod i n) where
 enumerated    = toMod <$> [0 .. fromInteger (natVal (Proxy :: Proxy n) - 1)]
 cardinality _ = fromInteger (natVal (Proxy :: Proxy n))
-}

{- | for non-'Generic' Bounded Enums:

@
instance Enumerable _ where
 'enumerated' = boundedEnumerated
 'cardinality' = 'boundedCardinality'
@

-}
boundedEnumerated :: (Bounded a, Enum a) => [a]
boundedEnumerated = enumFromTo minBound maxBound

{-| for non-'Generic' Bounded Enums.

behavior may be undefined when the cardinality of @a@ is larger than the cardinality of @Int@. this should be okay, as @Int@ is at least as big as @Int64@, which is at least as big as all the monomorphic types in @base@ that instantiate @Bounded@. you can double-check with:

>>> boundedCardinality (const(undefined::Int))   -- platform specific
18446744073709551616

@
-- i.e. 1 + 9223372036854775807 - (-9223372036854775808)
@

works with non-zero-based Enum instances, like @Int64@ or a custom @toEnum/fromEnum@.
assumes the enumeration's numbering is contiguous, e.g. if @fromEnum 0@ and @fromEnum 2@
both exist, then @fromEnum 1@ should exist too.

-}
boundedCardinality :: forall proxy a. (Bounded a, Enum a) => proxy a -> Natural
boundedCardinality _ = fromInteger (1 + (toInteger (fromEnum (maxBound::a))) - (toInteger (fromEnum (minBound::a))))

{- | for non-'Generic' Enums:

@
instance Enumerable ... where
 'enumerated' = enumEnumerated
@

the enum should still be bounded.

-}
enumEnumerated :: (Enum a) => [a]
enumEnumerated = enumFrom (toEnum 0)

{- | for non-'Generic' Bounded Indexed ('Ix') types:

@
instance Enumerable _ where
 'enumerated' = indexedEnumerated
 'cardinality' = 'indexedCardinality'
@

-}
indexedEnumerated :: (Bounded a, Ix a) => [a]
indexedEnumerated = range (minBound,maxBound)

{- | for non-'Generic' Bounded Indexed ('Ix') types.
-}
indexedCardinality :: forall proxy a. (Bounded a, Ix a) => proxy a -> Natural
indexedCardinality _ = int2natural (rangeSize (minBound,maxBound::a))

{-| enumerate only when the cardinality is small enough.
returns the cardinality when too large.

>>> enumerateBelow 2 :: Either Natural [Bool]
Left 2

>>> enumerateBelow 100 :: Either Natural [Bool]
Right [False,True]

useful when you've established that traversing a list below some length
and consuming its values is reasonable for your application.
e.g. after benchmarking, you think you can process a billion entries within a minute.

-}
enumerateBelow :: forall a. (Enumerable a) => Natural -> Either Natural [a]
enumerateBelow maxSize = if theSize < maxSize then Right enumerated else Left theSize
 where
 theSize = cardinality (Proxy :: Proxy a)

{-| enumerate only when completely evaluating the list doesn't timeout
(before the given number of microseconds).

>>> enumerateTimeout (2 * 10^6) :: IO (Maybe [Bool])  -- two seconds
Just [False,True]

-}
enumerateTimeout :: (Enumerable a, NFData a) => Int -> IO (Maybe [a])
enumerateTimeout maxDuration = timeout maxDuration (return$ force enumerated)
