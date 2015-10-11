{-# LANGUAGE DeriveGeneric, DefaultSignatures, TypeOperators, FlexibleInstances, FlexibleContexts, DeriveAnyClass, TypeFamilies, LambdaCase, EmptyCase, MultiParamTypeClasses  #-}

{- |

e.g. generate a boilerplate 'HasTrie' instance for a @Context@ type that has both sum types and product types 

@
data Context
 = GlobalContext | EmacsContext BufferName BufferMode BufferContents | OtherContext String
 deriving (Show,Generic) 
 -- Strings are infinite, so Context is too, that's okay 

instance HasDomain Context 

type BufferName = String
type BufferMode = String
type BufferContents = String

instance HasTrie Context where
 type a :->: b = ContextTrie ('TrieGeneric' a b)
 trie = 'trieGeneric'
 untrie = 'untrieGeneric'
@

the conversions (between a type @a@ and its representation @(Rep a x)@) make the generic instance 
slower than a manual instance.
but, if the function you're memoizing is costly enough, and datatype it consumes is messy enough, 
the four-line @HasTrie@ instance (and a one line call to 'memo') will save both runtime and developer time.


-}
module Data.Enumerate.Domain where 

-- import Data.MemoTrie

import           GHC.Generics


class GHasDomain f a where
 type GDomain f a :: (* -> *)
 -- type GDomain f :: (* -> *) -> * -> * -> (* -> *)
 gdomain :: f p -> (GDomain f a) p


-- | zero cases become (one case with) zero fields
instance GHasDomain (V1) a where
 type GDomain (V1) a = (U1)
 gdomain _ = (U1)


-- | one case becomes (one case with) one field
instance GHasDomain (U1) a where
 type GDomain (U1) a = (K1 () a)
 -- gdomain (U1) = (K1 a)
 gdomain (U1) = undefined 


-- -- | call 'domain'
-- instance (HasDomain a) => GHasDomain (K1 i a) where
--  type GDomain (K1 i a) = Domain a 
--  gdomain (K1 a) = domain a 


-- -- | two cases become (one case with) two fields
-- instance (GHasDomain (f), GHasDomain (g)) => GHasDomain (f :+: g) where
--  type GDomain (f :+: g) = Either (GDomain f) (GDomain g) 
--  gdomain (L1 f) = Left  (gdomain f) 
--  gdomain (R1 g) = Right (gdomain g) 


-- -- | two fields become (one case with) (one field of) two arrows
-- instance (GHasDomain (f), GHasDomain (g)) => GHasDomain (f :*: g) where
--  type GDomain (f :*: g) = ((GDomain f), (GDomain g))
--  gdomain (f :*: g) = (gdomain f, gdomain g)


-- -- | (ignore metadata) 
-- instance (GHasDomain (f)) => GHasDomain (M1 i a f) where
--  type GDomain (M1 i a f) =  (GDomain f)
--  gdomain (M1 f) = gdomain f


{- | 

e.g. 

@
a ~ Context

Rep a ~ 

GDomain (Rep a) ~ 

GCanonical (GDomain (Rep a)) ~ 

@

-}
-- type TrieGeneric a = GCanonical (GDomain (Rep a))

-- trieGeneric :: 
-- trieGeneric = 

-- untrieGeneric :: 
-- untrieGeneric = 

