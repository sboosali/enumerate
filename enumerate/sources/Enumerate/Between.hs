{-# LANGUAGE DataKinds, KindSignatures, GADTs, ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase, ScopedTypeVariables, TypeOperators, TupleSections #-}


{-| see 'Between', 'clipBetween', 'isBetween', 'readBetween'. 

-}
module Enumerate.Between where

import Enumerate.Types

import GHC.TypeLits

import Control.Exception
import Prelude (Enum(..))
import Prelude.Spiros


{- $setup

@
for doctest:
@

>>> :set -XDataKinds
>>> import Data.Proxy (Proxy(..))
>>> pBetweenNegativeThreeAndPositiveSeven = Proxy :: Proxy (Between Negative 3 Positive 7)

-}

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

-- | a hack since we can't distinguish @(Positive,0)@ from @(Negative,0)@
type Unsigned = Positive

class KnownSign (sign :: Sign) where
  signVal :: proxy sign -> Integer

-- | @= +1@
instance KnownSign 'Positive where
  signVal _ = 1

-- | @= -1@
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

{-|

>>> pNegative = Proxy :: Proxy Negative  -- 'Negative'
>>> pThree    = Proxy :: Proxy 3
>>> intVal pNegative pThree
-3

-}
intVal
  :: forall (sign :: Sign) (nat :: Nat) proxy proxy'.
     ( KnownSign sign
     , KnownNat  nat
     )
  => proxy  sign
  -> proxy' nat
  -> Integer
intVal pSign pNat =
  signVal pSign * natVal pNat


-- reifyINTEGER
--   :: forall (i :: INTEGER).
--     ( KnownINTEGER i
--     )
--   => proxy i
--   -> Integer
-- reifyINTEGER _ 


----------------------------------------

{-| An integer within an (inclusive) interval. 

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
 deriving (Show,Eq,Ord,Generic,Data,NFData,Hashable)

----------------------------------------

-- | @[0 .. 7]@
type Octal = Base 8

-- | @[0 .. 9]@
type Decimal = Base 10  -- Between Unsigned 0 Positive 9

-- | @[0 .. 15]@
type Hexadecimal = Base 16

----------------------------------------

-- | @[0 .. 100]@
type Percentage = PositiveBelow 100

----------------------------------------

-- | @[-1, 0, +1]@
type PlusMinusOne = Absolute 1

----------------------------------------

-- | @[-n .. +n]@
type Absolute (n :: Nat) = Between Negative n Positive n
 
-- | @[0 .. (n-1)]@
type Base (n :: Nat) = PositiveBelow (n-1)

-- | @[-n .. 0]@
type NegativeBelow (n :: Nat) = Between Negative n Unsigned 0

-- | @[0 .. +n]@
type PositiveBelow (n :: Nat) = Between Unsigned 0 Positive n

----------------------------------------

-- | an out-of-bounds 'toEnum' is clipped (with 'clipBetween'). 
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

-- | all operations that overflow/underflow are clipped (with 'clipBetween'). 
instance
  ( KnownInteger sin min
  , KnownInteger sax max
  ) => Num (Between sin min sax max)
  where

  fromInteger = fromInteger >>> idInteger >>> clipBetween'
    where
    idInteger :: Integer -> Integer
    idInteger = id

  UnsafeBetween i₁ + UnsafeBetween i₂ = clipBetween' $
    i₁ + i₂
    
  UnsafeBetween i₁ * UnsafeBetween i₂ = clipBetween' $
    i₁ * i₂

  abs    (UnsafeBetween i) = clipBetween' $
    abs i
    
  signum (UnsafeBetween i) = clipBetween' $
    signum i
    
  negate (UnsafeBetween i) = clipBetween' $
    negate i

-- | all operations that overflow/underflow are clipped (with 'clipBetween'). 
instance
  ( KnownInteger sin min
  , KnownInteger sax max
  ) => Real (Between sin min sax max)
  where
    
  toRational (UnsafeBetween i) = toRational i

-- | all operations that overflow/underflow are clipped (with 'clipBetween'). 
instance
  ( KnownInteger sin min
  , KnownInteger sax max
  ) => Integral (Between sin min sax max)
  where
    
  toInteger (UnsafeBetween i) = i
  
  quotRem (UnsafeBetween i) (UnsafeBetween j) =
    (clipBetween' *** clipBetween') (i `quotRem` j)
  
  {-
   quot :: a -> a -> a 
        integer division truncated toward zero
   rem :: a -> a -> a 
       integer remainder
  -}

-- | @read \@Integer@ and @'checkBetween'@
instance
  ( KnownBetween sin min sax max
  ) =>
  Read (Between sin min sax max) where
  
  readsPrec precedence string = results'
  
     where
     results' :: [((Between sin min sax max), String)]
     results' = results & fmap go & catMaybes
       -- fwiw, we don't short-circuit, we just keep the parses whose integers are in-bounds, but they're probably equivalent 

     go ::       (Integer,                 String)
        -> Maybe (Between sin min sax max, String)
     go (i,s) = (,s) <$> either2maybe (isBetween' i)
     
     results :: [(Integer, String)]
     results = readsPrec precedence string

  {-
*Main> :i readsPrec
class Read a where
  readsPrec :: Int -> ReadS a
  ...
    -- Defined in GHC.Read
*Main> :i ReadS
type ReadS a = String -> [(a, String)]
    -- Defined in Text.ParserCombinators.ReadP
  -}
  
----------------------------------------

{-|

>>> minBetween pBetweenNegativeThreeAndPositiveSeven
-3

-}
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

{-|

>>> maxBetween pBetweenNegativeThreeAndPositiveSeven
7

-}
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

{-|

>>> rangeBetween pBetweenNegativeThreeAndPositiveSeven
10

-}
rangeBetween
  :: forall sin min sax max proxy.
    ( KnownBetween sin min sax max
    )
  => proxy (Between sin min sax max)
  -> Natural
rangeBetween proxy
    = maximum' - minimum'
    & max 0
    & fromInteger
  where
  maximum' = maxBetween proxy
  minimum' = minBetween proxy

----------------------------------------

{- | the primary constructor. total via clipping (rounds up underflow and rounds down overflow).

i.e. 'id' on in-bounds inputs, and 'min' or 'max' for out-of-bounds inputs.

>>> clipBetween pBetweenNegativeThreeAndPositiveSeven (-9 :: Integer)
UnsafeBetween (-3)
>>> clipBetween pBetweenNegativeThreeAndPositiveSeven (9 :: Integer)
UnsafeBetween 7
>>> clipBetween pBetweenNegativeThreeAndPositiveSeven (0 :: Integer)
UnsafeBetween 0

-}
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
  >>> max (minBetween proxy)
  >>> min (maxBetween proxy)
  >>> UnsafeBetween
  -- where
  -- proxy = Proxy :: Proxy (Between sin min sax max)

-- | is 'clipBetween' without a proxy (it leans on inference). 
clipBetween'
  :: forall a sin min sax max.
     ( Integral a -- Num a
     , KnownBetween sin min sax max
     )
  => a
  -> Between sin min sax max
clipBetween' = clipBetween Nothing

----------------------------------------
--TODO specializations

{-# SPECIALIZE clipBetween
               :: Maybe (Between Unsigned 0 Positive 9)
               -> Integer
               -> (Between Unsigned 0 Positive 9)
  #-}

{-# SPECIALIZE clipBetween
               :: Proxy (Between Unsigned 0 Positive 9)
               -> Integer
               -> (Between Unsigned 0 Positive 9)
  #-}

{-# SPECIALIZE clipBetween
               :: Maybe (Between Unsigned 0 Positive 9)
               -> Int
               -> (Between Unsigned 0 Positive 9)
  #-}

{-# SPECIALIZE clipBetween
               :: Proxy (Between Unsigned 0 Positive 9)
               -> Int
               -> (Between Unsigned 0 Positive 9)
  #-}


{-# SPECIALIZE clipBetween
               :: Maybe (Between Unsigned 0 Positive 255)
               -> Integer
               -> (Between Unsigned 0 Positive 255)
  #-}

{-# SPECIALIZE clipBetween
               :: Proxy (Between Unsigned 0 Positive 255)
               -> Integer
               -> (Between Unsigned 0 Positive 255)
  #-}

{-# SPECIALIZE clipBetween
               :: Maybe (Between Unsigned 0 Positive 255)
               -> Int
               -> (Between Unsigned 0 Positive 255)
  #-}

{-# SPECIALIZE clipBetween
               :: Proxy (Between Unsigned 0 Positive 255)
               -> Int
               -> (Between Unsigned 0 Positive 255)
  #-}

---------------------------------------
  
{- | a secondary constructor. partial (safely), via @Either 'NotBetween'@).

outputs:

* 'Right' on in-bounds inputs,
* 'NumberIsTooLow' or 'NumberIsTooHigh' for out-of-bounds inputs,
* 'IntervalIsEmpty' when the 'Between' itself is invalid. 

>>> isBetween pBetweenNegativeThreeAndPositiveSeven (-9 :: Integer)
Left NumberIsTooLow
>>> isBetween pBetweenNegativeThreeAndPositiveSeven (9 :: Integer)
Left NumberIsTooHigh
>>> isBetween pBetweenNegativeThreeAndPositiveSeven (0 :: Integer)
Right (UnsafeBetween 0)

>>> import Prelude (undefined)
>>> pEmptyBetween = Proxy :: Proxy (Between Positive 1 Negative 1)
>>> isBetween pEmptyBetween undefined
Left IntervalIsEmpty

-}
isBetween
  :: forall a sin min sax max proxy.
     ( Integral a -- Num a
     , KnownBetween sin min sax max
     )
  => proxy (Between sin min sax max)
  -> a
  -> Either NotBetween (Between sin min sax max)
isBetween proxy x =
  if   not (checkBetween proxy)
  then Left IntervalIsEmpty
  else if   not (i >= minimum')
       then Left NumberIsTooLow
       else if   not (i <= maximum')
            then Left NumberIsTooHigh
            else Right (UnsafeBetween i)
  where
  i        = toInteger x
  maximum' = maxBetween proxy
  minimum' = minBetween proxy

-- | 'isBetween' with the type parameters being implicit (i.e. no @proxy@). 
isBetween'
  :: forall a sin min sax max.
     ( Integral a -- Num a
     , KnownBetween sin min sax max
     )
  => a
  -> Either NotBetween (Between sin min sax max)
isBetween' = isBetween Nothing

----------------------------------------

data NotBetween
  = NumberIsTooLow
  | NumberIsTooHigh
  | IntervalIsEmpty
  deriving (Show,Read,Eq,Ord,Enum,Bounded,Generic)

instance Exception  NotBetween
instance NFData     NotBetween
instance Hashable   NotBetween
instance Enumerable NotBetween

{- | 'False' if the interval itself ('Between') is invalid:
'minBetween' must be less than or equal to 'maxBetween'

>>> pSingularBetween = Proxy :: Proxy (Between Positive 1 Positive 1)
>>> checkBetween pSingularBetween
True
>>> checkBetween (Proxy :: Proxy (Between Positive 1 Positive 2))
True
>>> checkBetween (Proxy :: Proxy (Between Positive 1 Negative 1))
False

Can be checked at the type-level too.

-}
checkBetween  
  :: forall sin min sax max proxy.
     ( KnownBetween sin min sax max
     )
  => proxy (Between sin min sax max)
  -> Bool
checkBetween proxy = minimum' <= maximum'
  where
  minimum' = minBetween proxy
  maximum' = maxBetween proxy


--NOTE uselessly uninferrable
-- -- | 'checkBetween' with the type parameters being implicit (i.e. no @proxy@).
-- checkBetween'  
--   :: forall sin min sax max proxy.
--      ( KnownBetween sin min sax max
--      )
--   => Bool
-- checkBetween' = checkBetween Nothing

---------------------------------------

{-|

>>> readBetween pBetweenNegativeThreeAndPositiveSeven "11" 
Left (BetweenValidationFailure NumberIsTooHigh)
>>> readBetween pBetweenNegativeThreeAndPositiveSeven "three" 
Left (BetweenParsingFailure "three")
>>> readBetween pBetweenNegativeThreeAndPositiveSeven "-1"
Right (UnsafeBetween (-1))

-}
readBetween
  :: forall sin min sax max proxy.
     ( KnownBetween sin min sax max
     )
  => proxy (Between sin min sax max)
  -> String
  -> Either BetweenError (Between sin min sax max)
readBetween proxy s = do
  i <- readInteger_BetweenError s
  j <- isBetween_BetweenError i
  return j

  where
  isBetween_BetweenError
    = isBetween proxy
    > first BetweenValidationFailure
  
  readInteger_BetweenError
    = readInteger
    > maybe2either (BetweenParsingFailure s)

  readInteger :: String -> Maybe Integer
  readInteger = readMay

data BetweenError
  = BetweenParsingFailure    String
  | BetweenValidationFailure NotBetween
  deriving (Show,Read,Eq,Ord,Generic)

instance Exception  BetweenError
instance NFData     BetweenError
instance Hashable   BetweenError
--instance Enumerable BetweenError

-- | 'readBetween' with the type parameters being implicit (i.e. no @proxy@).
readBetween'
  :: forall sin min sax max.
     ( KnownBetween sin min sax max
     )
  => String
  -> Either BetweenError (Between sin min sax max)
readBetween' = readBetween Nothing
  
----------------------------------------


{-

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




>>> readBetween' "11" :: Between Negative 10 Positive 10 
Left (BetweenValidationFailure NumberIsTooHigh)
>>> readBetween' "eleven" :: Between Negative 10 Positive 10
Left (BetweenParsingFailure "eleven")
>>> readBetween' "-5" :: Between Negative 10 Positive 10 
Right (UnsafeBetween 5)



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
