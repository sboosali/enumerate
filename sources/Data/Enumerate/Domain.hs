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

but, if the function you're memoizing is costly enough, and the datatype it consumes is messy enough, 
the four-line @HasTrie@ instance (and a one line call to 'memo') can save both runtime and developer time.


-}
module Data.Enumerate.Domain where 
import Data.CoRec

import Data.MemoTrie

import           GHC.Generics


-- class GHasTrie f a where
--  type GTrie f a :: (* -> *)
--  -- type GTrie f :: (* -> *) -> * -> * -> (* -> *)
--  gtrie :: f p -> (a -> ) -> (GTrie f a) p
--  guntrie :: (GTrie f a) p -> (a -> )


-- -- | zero cases become (one case with) zero fields
-- instance GHasTrie (V1) a where
--  type GTrie (V1) a = (U1)
--  gtrie _ = (U1)


-- -- | one case becomes (one case with) one field
-- instance GHasTrie (U1) a where
--  type GTrie (U1) a = (K1 () a)
--  -- gtrie (U1) = (K1 a)
--  gtrie (U1) = undefined 

 
-- -- | call 'trie'
-- instance (HasTrie a) => GHasTrie (K1 i a) where
--  type GTrie (K1 i a) = Trie a 
--  gtrie (K1 a) = trie a 


-- -- -- | two cases become (one case with) two fields
-- -- instance (GHasTrie (f), GHasTrie (g)) => GHasTrie (f :+: g) where
-- --  type GTrie (f :+: g) = Either (GTrie f) (GTrie g) 
-- --  gtrie (L1 f) = Left  (gtrie f) 
-- --  gtrie (R1 g) = Right (gtrie g) 


-- -- -- | two fields become (one case with) (one field of) two arrows
-- -- instance (GHasDomain (f), GHasDomain (g)) => GHasDomain (f :*: g) where
-- --  type GDomain (f :*: g) = ((GDomain f), (GDomain g))
-- --  gdomain (f :*: g) = (gdomain f, gdomain g)


-- -- -- | (ignore metadata) 
-- -- instance (GHasDomain (f)) => GHasDomain (M1 i a f) where
-- --  type GDomain (M1 i a f) =  (GDomain f)
-- --  gdomain (M1 f) = gdomain f


-- {- | 

-- e.g. 

-- @
-- a ~ Context

-- Rep a ~ 

-- GDomain (Rep a) ~ 

-- GCanonical (GDomain (Rep a)) ~ 

-- @

-- -}
-- -- type TrieGeneric a = GCanonical (GDomain (Rep a))

-- -- trieGeneric :: 
-- -- trieGeneric = 

-- -- untrieGeneric :: 
-- -- untrieGeneric = 

