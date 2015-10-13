{-# LANGUAGE TypeOperators, DeriveDataTypeable, LambdaCase, TypeFamilies, UndecidableInstances, ScopedTypeVariables, FlexibleContexts, GADTs, ConstraintKinds, DataKinds #-}

{-| 

-}
module Data.CoRec.MemoTrie where 
import Data.CoRec

import Data.MemoTrie
import Data.Vinyl
import Data.Vinyl.TypeLevel
import Data.Vinyl.Functor

import           GHC.Generics
import GHC.Prim (Constraint)
import Data.Proxy (Proxy(..)) 


{-| 

-}
instance (RecAll f as HasTrie, RecApplicative as) => HasTrie (CoRec f as) where
 newtype (CoRec f as) :->: b = CoRecTrie { unCoRecTrie :: (Rec ((:<-:) b :. f) as) }
 trie   = undefined -- CoRecTrie . trieCoRec 
 untrie = undefined -- untrieCoRec . unCoRecTrie

newtype (b :<-: a) = OpTrie (a :->: b)

-- | a flipped '∈'. 
type (∋) rs r = RElem r rs (RIndex r rs)

{-| 

@('RecAll' f as HasTrie)@ means if @(a ∈ as)@ then @(HasTrie (f a))@. 

generalizes:

@
instance (HasTrie a, HasTrie b) => HasTrie (Either a b) where
  trie f = 'EitherTrie' (trie (f . Left)) (trie (f . Right))
  ...
@


-}
-- trieCoRec :: forall f as b. (RecAll f as HasTrie, RecApplicative as) => (CoRec f as -> b) -> (Rec ((:<-:) b :. f) as)
-- trieCoRec f = rpure (Compose (OpTrie (trie (f . Col))))
--  where

-- example = reifyConstraints (Proxy :: Proxy ((∋) rs)) (Proxy :: Proxy HasTrie) 

{-| 

-}
-- untrieCoRec :: (RecAll f as HasTrie) => (Rec ((:<-:) b) as) -> (CoRec f as -> b)
-- untrieCoRec tries = f
--  where
--  f = undefined

{-| like 'Dict', but can reify constraints on the raw types as well as the items.

-}
data Dicts cType cItem f a where
  Dicts
    :: (cType a, cItem (f a))
    => !(f a)
    -> Dicts cType cItem f a

{-| A constraint on each type in a type-level list.

-}
type family EachHas ts c :: Constraint where
  EachHas '[] c = ()
  EachHas (t ': ts) c = (c t, EachHas ts c)

-- {-| like 'reifyConstraint', but can reify constraints on the raw types as well as the items.

-- e.g. given @Rec Maybe [Bool,String]@, it can reify @(cType Bool, cType String, cItem (Maybe Bool), cItem (Maybe String))@. 


-- -}
-- reifyConstraints
--   :: (EachHas as cType, RecAll f as cItem)
--   => proxy cType
--   -> proxy cItem
--   -> Rec f as
--   -> Rec (Dicts cType cItem f :. f) as
-- reifyConstraints cType cItem = \case 
--     RNil -> RNil
--     (x :& xs) -> Compose (Dicts x) :& reifyConstraints cType cItem xs 

