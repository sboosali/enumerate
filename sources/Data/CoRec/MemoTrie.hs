{-# LANGUAGE TypeOperators, DeriveDataTypeable, LambdaCase, TypeFamilies, UndecidableInstances, ScopedTypeVariables, FlexibleContexts  #-}

{-| 

-}
module Data.CoRec.MemoTrie where 
import Data.CoRec

import Data.MemoTrie
import Data.Vinyl
import Data.Vinyl.TypeLevel
import Data.Vinyl.Functor

import           GHC.Generics


{-| 

-}
instance (RecAll f as HasTrie, RecApplicative as) => HasTrie (CoRec f as) where
 -- newtype (CoRec f as) :->: b = CoRecTrie { unCoRecTrie :: (Rec ((:<-:) b :. (Dict HasTrie)) as) }
 newtype (CoRec f as) :->: b = CoRecTrie { unCoRecTrie :: (Rec ((:<-:) b :. f) as) }
 trie   = CoRecTrie . trieCoRec 
 untrie = untrieCoRec . unCoRecTrie

newtype (b :<-: a) = OpTrie (a :->: b)

-- reifyConstraint (Proxy :: Proxy HasTrie) 

{-| 

@('RecAll' f as HasTrie)@ means if @(a ∈ as)@ then @(HasTrie (f a))@. 

generalizes:

@
instance (HasTrie a, HasTrie b) => HasTrie (Either a b) where
    trie f = 'EitherTrie' (trie (f . Left)) (trie (f . Right))
@


-}
trieCoRec :: forall f as b. (RecAll f as HasTrie, RecApplicative as) => (CoRec f as -> b) -> (Rec ((:<-:) b :. f) as)
-- trieCoRec f = rpure (colTrie f)
trieCoRec f = rpure oneTrie 
 where
 -- oneTrie :: forall a. (a ∈ as) => (b :<-: (Dict HasTrie (f a)))
 -- oneTrie :: (b :<-: (Dict HasTrie (f a)))
 -- oneTrie :: forall a. (b :<-: (Dict HasTrie (f a)))
 oneTrie :: forall a. (a ∈ as, HasTrie (f a)) => ((:<-:) b :. f) a
 oneTrie = (colTrie (f . Col))

-- type ElemHasTrie f as a = ((a ∈ as), (HasTrie (f a)))

-- {-| 

-- -}
-- colTrie :: ((Dict HasTrie (f a)) -> b) -> (b :<-: (Dict HasTrie (f a)))
-- colTrie f = (OpTrie (trie f))

-- colTrie :: forall f a b. (HasTrie (f a)) => (f a -> b) -> (b :<-: f a)
-- colTrie f = (OpTrie (trie f))

colTrie :: forall f a b. (HasTrie (f a)) => (f a -> b) -> ((:<-:) b :. f) a
colTrie f = Compose (OpTrie (trie f))

-- getDict :: c a => Dict c a -> a
-- getDict (Dict a) = a

-- untrieCoRec :: (RecAll f as HasTrie) => (Rec ((:<-:) b) as) -> (CoRec f as -> b)
untrieCoRec tries = f
 where
 f = undefined 

