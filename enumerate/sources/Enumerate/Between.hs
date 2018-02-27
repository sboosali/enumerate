{-# LANGUAGE DataKinds, KindSignatures, GADTs, ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

module Enumerate.Between where

import Enumerate.Types

import GHC.TypeLits

import Prelude (Enum(..))
import Prelude.Spiros

----------------------------------------
-- data Modular i = Modular

----------------------------------------

{-

-- | kind
data Signed
 = Positive
 | Negative

-}

type KnownBetween (sin :: Sign) (min :: Nat) (sax :: Sign) (max :: Nat) =
  ( KnownInteger sin min
  , KnownInteger sax max
  )

type KnownInteger (sign :: Sign) (nat :: Nat) = 
     ( KnownSign sign
     , KnownNat  nat
     )

----------------------------------------

-- | kind
data Sign
 = Positive
 | Negative

class KnownSign (sign :: Sign) where
  signVal :: proxy sign -> Integer

instance KnownSign 'Positive where
  signVal _ = 1

instance KnownSign 'Negative where
  signVal _ = -1

{-

-- | singleton 
data Sign_ (sign :: Sign) where
  Positive_ :: Sign_ 'Positive
  Negative_ :: Sign_ 'Negative

sign2int :: Sign_ sign -> Integer
sign2int = \case
  Positive_ -> -1
  Negative_ ->  1 

-}

intVal
  :: forall (sign :: Sign) (nat :: Nat) proxy proxy'.
     ( KnownSign sign
     , KnownNat  nat
     )
  => proxy sign
  -> proxy' nat
  -> Integer
intVal sign nat = signVal sign * natVal nat





-- reifyINTEGER
--   :: forall (i :: INTEGER).
--     ( KnownINTEGER i
--     )
--   => proxy i
--   -> Integer
-- reifyINTEGER _ 


----------------------------------------

{-| a integer in an inclusive interval. 


@
Between sin min sax max
@

* @sin@: sign for the minimum
* @sax@: sign for the maximum
* @min@: the minimum bound
* @max@: the maximum bound


e.g. to represent [-3..7]

@
Between 'Negative' 3 'Positive' 7
@


e.g. forall (s :: Sign) (n :: Nat)

@
Between s n s n
~
Singleton (intVal (Proxy @s) (Proxy @n))
@


-}
newtype Between
 (sin :: Sign) (min :: Nat)
 (sax :: Sign) (max :: Nat)
 = UnsafeBetween Integer
 deriving (Show,Eq,Ord,Generic,NFData)

----------------------------------------

instance
  ( KnownBetween sin min sax max
  ) => Enum (Between sin min sax max) where
  
  fromEnum (UnsafeBetween i) = fromInteger i
  
  toEnum = clipBetween (Proxy :: Proxy (Between sin min sax max))

instance
  ( KnownInteger sin min
  , KnownInteger sax max
  ) => Bounded (Between sin min sax max)
  where
    
  minBound = UnsafeBetween $
    minBetween (Proxy :: Proxy (Between sin min sax max))

  maxBound = UnsafeBetween $
    maxBetween (Proxy :: Proxy (Between sin min sax max))

instance
  ( KnownInteger sin min
  , KnownInteger sax max
  ) => Enumerable (Between sin min sax max)
  where
  enumerated = [minBound .. maxBound]

--instance (Integral i, KnownNat n) => Num (i `Mod` n) where

instance
  ( KnownInteger sin min
  , KnownInteger sax max
  ) => Num (Between sin min sax max)
  where

  fromInteger = fromInteger >>> idInteger >>> clipBetween Nothing
    where
    idInteger :: Integer -> Integer
    idInteger = id

  UnsafeBetween i₁ + UnsafeBetween i₂ = clipBetween Nothing $ i₁ + i₂
  UnsafeBetween i₁ * UnsafeBetween i₂ = clipBetween Nothing $ i₁ * i₂

  abs    (UnsafeBetween i) = clipBetween Nothing $ abs i
  signum (UnsafeBetween i) = clipBetween Nothing $ signum i
  negate (UnsafeBetween i) = clipBetween Nothing $ negate i

----------------------------------------

minBetween
  :: forall sin min sax max proxy.
     ( KnownInteger sin min
     )
  => proxy (Between sin min sax max)
  -> Integer
minBetween _proxy = intVal pSin pMin
  where
  pSin = Proxy :: Proxy sin
  pMin = Proxy :: Proxy min

maxBetween
  :: forall sin min sax max proxy.
    ( KnownInteger sax max
    )
  => proxy (Between sin min sax max)
  -> Integer
maxBetween _proxy = intVal pSax pMax
  where
  pSax = Proxy :: Proxy sax
  pMax = Proxy :: Proxy max

-- | primary constructor. 
clipBetween
  :: forall a sin min sax max proxy.
     ( Integral a -- Num a
     , KnownBetween sin min sax max
     )
  => proxy (Between sin min sax max)
  -> a
  -> Between sin min sax max
clipBetween proxy
  = toInteger
  > max (minBetween proxy)
  > min (maxBetween proxy)
  > UnsafeBetween
  -- where
  -- proxy = Proxy :: Proxy (Between sin min sax max)

----------------------------------------

---------------------------------------


{-

----------------------------------------

data INTEGER
 = Negative Nat
 | Positive Nat

type family KnownINTEGER (i :: INTEGER) :: Constraint where
  KnownINTEGER (Negative n) = (KnownNat n)
  KnownINTEGER (Positive n) = (KnownNat n)

reifyINTEGER
  :: forall (i :: INTEGER).
    ( KnownINTEGER i
    )
  => proxy i
  -> Integer
reifyINTEGER _ 
  
----------------------------------------

newtype Between (min :: INTEGER) (max :: INTEGER) = Between Integer
 deriving (Show,Eq,Ord,Generic,NFData)

minBetween :: Between a => a -- Between
minBetween = -5

maxBetween :: Num a => a -- Between
maxBetween = 25

clipBetween :: Int -> Between
clipBetween = max minBetween > min maxBetween > Between

instance Enum Between where
  fromEnum (Between i) = i
  toEnum = clipBetween

instance Bounded Between where
  minBound = minBetween
  maxBound = maxBetween

instance Enumerable Between where
  enumerated = [minBetween .. maxBetween]

--instance (Integral i, KnownNat n) => Num (i `Mod` n) where

instance Num Between where
  fromInteger = fromInteger > clipBetween

  Between i₁ + Between i₂ = clipBetween $ i₁ + i₂
  Between i₁ * Between i₂ = clipBetween $ i₁ * i₂

  abs    (Between i) = clipBetween $ abs i
  signum (Between i) = clipBetween $ signum i
  negate (Between i) = clipBetween $ negate i

---------------------------------------






----------------------------------------

{-| 

-}
newtype Between i j a
  = Between a

--minimumBetween

-- TODO @a@ is any @Ord@erable, @i@ and @j@ would require reflection
  
instance (KnownNat i, KnownNat j) => Bounded (Between i j) where
  minBound = _
  maxBound = _

-}
