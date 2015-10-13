{-# LANGUAGE BangPatterns,
             ConstraintKinds,
             DataKinds,
             FlexibleContexts,
             FlexibleInstances,
             GADTs,
             KindSignatures,
             MultiParamTypeClasses,
             RankNTypes,
             ScopedTypeVariables,
             TypeOperators,
             UndecidableInstances #-}
{-| 

-}
module Data.CoRec where 

import Data.Vinyl
import Data.Vinyl.Functor (Compose(..), (:.), Identity(..))
import Data.Vinyl.TypeLevel (RIndex)
import Data.Vinyl.Lens (rget) 

import Data.Maybe(fromJust)
import Data.Proxy
-- import GHC.Prim (Constraint)


-- | Generalize algebraic sum types.
data CoRec :: (* -> *) -> [*] -> * where
  Col :: (t ∈ ts) => !(f t) -> CoRec f ts

-- | a value of type @OneOf [x,y,z]@ holds a value of one of the types, either @x@ or @y@ or @z@.   
type OneOf = CoRec Identity

instance Show (CoRec (Dict Show) ts) where
  show (Col (Dict x)) = "Col "++show x

{-| A function type constructor that takes its arguments in the reverse order.

without type lambdas, we need a type constructor to reorder type parameters. 
-}
newtype Outputs b a = O { runOutputs :: a -> b }

{-| @(b <-- a)@ is isomorphic to @(a -> b)@.  

-}
type (<--) = Outputs

{-| a flipped @CoKleisli@. 

-}
newtype Handler f b a = H { runHandler :: f a -> b }



-- ================================================================ --

-- | helper to build a 'OneOf'
column :: (t ∈ ts) => t -> OneOf ts
column = Col . Identity 
{-# INLINEABLE column #-}

{-| Handler generalizes Outputs.  

-}
outputs2handler :: Outputs b a -> Handler Identity b a
outputs2handler (O f) = H (f.getIdentity)
{-# INLINEABLE outputs2handler #-}


{-| you consume a coproduct with a product of consumers i.e. you must handle every case.

generalizes 'match'. 

-}
handle :: Rec (Handler f b) as -> CoRec f as -> b
handle handlers (Col variant) = h variant
 where
 H h = rget variant handlers
 -- pattern matching refines the type,
 -- which we index into the handler array,
 -- (@f a@ acts unifies with @proxy a@), 
 -- to access the correct handler. 
{-# INLINEABLE handle #-}


{-| perform "pattern matching" upon a CoRec. 

a generalized, uncurried, newtyped version of: 

@
'either' :: (a1 -> b) -> (a2 -> b) -> Either a b
@

e.g. 

>>> :set +m
>>> import Data.Vinyl

truthy is like a final method (i.e. different definitions can be dispatched on a fixed set of different types):

>>> :{
let truthy :: Rec (Outputs Bool) [Int, String, Bool]
    truthy =  (O$ \i -> i==0)
           :& (O$ \s -> s=="")
           :& (O$ \b -> b)
           :& RNil
:}

>>> match truthy (column True       :: OneOf [Int,String,Bool]) 
True
>>> match truthy (column "nonempty" :: OneOf [Int,String,Bool])
True
>>> match truthy (column (1::Int)   :: OneOf [Int,String,Bool])
True

the integer literal needs a type signature (numeric literals are overloaded) because Haskell can't prove that @(Num t, t ∈ [Int,String,Bool])@. but it can prove @(Int ∈ [Int,String,Bool])@. with @ExtendedDefaultRules@, the type variables can be defaulted. the same problem (and solutions) exists for string literals with @OverloadedStrings@, and for list literals with @OverloadedLists@. 

type inference does work when the 'column' is immediately consumed@: 

>>> match truthy (column False) 
False
>>> match truthy (column "") 
False
>>> match truthy (column (0::Int)) 
False

-}
match
 :: Rec (Outputs b) as
 -> OneOf as
 -> b
match hs c = handle (rmap outputs2handler hs) c
{-# INLINEABLE match #-}



-- ================================================================ --

{- examples 

truthyHandlers :: Rec (Handler Identity Bool) [Int, String, Bool]
truthyHandlers 
  = (H$ \(Identity i) -> i==0)
 :& (H$ \(Identity s) -> s=="")
 :& (H$ \(Identity b) -> b)
 :& RNil
-- truthyHandlers = rmap (H :: forall f b a. (f a -> b) -> (Handler f b a))

truthyMatchers :: Rec (Outputs Bool) [Int, String, Bool]
truthyMatchers
 =  (O$ \i -> i==0)
 :& (O$ \s -> s=="")
 :& (O$ \b -> b)
 :& RNil

falsyZero  = handle truthyHandlers (column (0::Int)    :: OneOf [Int, String, Bool])
falsyEmpty = handle truthyHandlers (column ""          :: OneOf [Int, String, Bool])
falsyFalse = handle truthyHandlers (column False       :: OneOf [Int, String, Bool])

truthyOne  = match truthyMatchers (column (1::Int)     :: OneOf [Int,String,Bool]) 
truthyFull = match truthyMatchers (column "full"       :: OneOf [Int,String,Bool]) 
truthyTrue = match truthyMatchers (column True         :: OneOf [Int,String,Bool]) 

falsyZero'  = handle truthyHandlers (column (0::Int))
falsyEmpty' = handle truthyHandlers (column "")
falsyFalse' = handle truthyHandlers (column False)

truthyOne'  = match truthyMatchers (column (1::Int))
truthyFull' = match truthyMatchers (column "full")
truthyTrue' = match truthyMatchers (column True)

-} 

