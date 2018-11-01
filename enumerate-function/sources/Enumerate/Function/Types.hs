{-# LANGUAGE RankNTypes, DeriveAnyClass, TypeOperators #-}

--------------------------------------------------
--------------------------------------------------

module Enumerate.Function.Types where

--------------------------------------------------

import Enumerate.Types
import Enumerate.Function.Extra

--------------------------------------------------
--------------------------------------------------

import "exceptions" Control.Monad.Catch (MonadThrow)

--------------------------------------------------

-- import "deepseq" Control.DeepSeq

--------------------------------------------------

import "base" Data.Ix (Ix)

--------------------------------------------------
--------------------------------------------------

{-| Used by "Enumerate.Function.Reify.getJectivityM".

-}

data Jectivity

  = Injective
  | Surjective
  | Bijective

 deriving
   ( Enum, Bounded, Ix
   , Show, Read
   , Eq, Ord
   , Generic, Data
   , NFData, Enumerable
   )

--------------------------------------------------
--------------------------------------------------

{- with proof:

the signature of the inverse of (a -> b)

data Jectivity a b
 = Unjective  (b -> [a])
 | Injective  (b -> Maybe a)
 | Surjective (b -> NonEmpty a)
 | Bijective  (b -> a)

data Jectivity_ = Injective_ | Surjective_ | Bijective_

jectivity :: () => (a -> b) -> Jectivity a b

jectivity_ :: Jectivity -> Maybe Jectivity_

OR

newtype Injection  a b = Injection  (a -> b) (b -> Maybe a)
newtype Surjection a b = Surjection (a -> b) (b -> NonEmpty a)
newtype Bijection  a b = Bijection  (a -> b) (b -> a)

-- | each input has zero-or-one output
newtype a :?->: b = Injection  (a -> b) (b -> Maybe a)
-- | each input has one-or-more output
newtype a :+->: b = Surjection (a -> b) (b -> NonEmpty a)
-- | each input has one output
newtype a :<->: b = Bijection  (a -> b) (b -> a)

toInjection  :: (a -> b) -> Maybe (Injection  a b)
toSurjection :: (a -> b) -> Maybe (Surjection a b)
toBijection  :: (a -> b) -> Maybe (Bijection  a b)

asInjection :: (a :<->: b) -> (a :?->: b)
asInjection (Bijection f g) = Injection f (Just <$> g) -- pure

asSurjection :: (a :<->: b) -> (a :+->: b)
asSurjection (Bijection f g) = Surjection f ((:|[]) <$> g) -- pure


-}

--------------------------------------------------
--------------------------------------------------

{-| A /safely/-partial function.

i.e. a function that:

* Fails only via the 'throwM' method of 'MonadThrow'.
* Succeeds only via the 'return' method of 'Monad'.

-}

type Partial a b = (forall m. MonadThrow m => a -> m b)

--------------------------------------------------
--------------------------------------------------

{-| Operator (infix) for 'Partial'.

NAMING: Looks like the "function arrow" @(->)@.

-}

type (a -?> b) = Partial a b

--------------------------------------------------
--------------------------------------------------

 -- (by necessity) @'KnownNat' ('Cardinality' a)@
 --class (KnownNat (Cardinality a)) => Enumerable a where

  -- type Cardinality a :: Nat -- TODO
  {- too much boilerplate

   e.g.

  instance Enumerable Jectivity

  errors with:

  No instance for (KnownNat (Cardinality Jectivity))
   arising from the superclasses of an instance declaration
  In the instance declaration for `Enumerable Jectivity'

  would need:

  instance (KnownNat (Cardinality Jectivity)) => Enumerable Jectivity

  -}

--------------------------------------------------