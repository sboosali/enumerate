{-# LANGUAGE TypeOperators, LambdaCase, EmptyCase, TypeFamilies, FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric  #-}-- TODO example module 

module Data.MemoTrie.Generic where 

import Data.MemoTrie

import           GHC.Generics
import Control.Arrow (first) 


{-| "unlifted" generic representation. (i.e. is a nullary type constructor). 

-}
type Reg a = Rep a () 

instance HasTrie (V1 x) where
 data (V1 x :->: b) = V1Trie 
 trie f = V1Trie 
 untrie V1Trie = \case
 enumerate V1Trie = [] 

instance HasTrie (U1 x) where
 data (U1 x :->: b) = U1Trie b 
 trie f = U1Trie (f U1)
 untrie (U1Trie b) = \case
  U1 -> b                       -- TODO strictness? 
 enumerate (U1Trie b) = [(U1, b)] 

instance (HasTrie (f x), HasTrie (g x)) => HasTrie ((f :+: g) x) where
 newtype ((f :+: g) x :->: b) = EitherTrie1 (Either (f x) (g x) :->: b)
 trie f = EitherTrie1 (trie (f . liftSum))
 untrie (EitherTrie1 t) = (untrie t) . dropSum
 enumerate (EitherTrie1 t) = _enumerateWith liftSum t

instance (HasTrie (f x), HasTrie (g x)) => HasTrie ((f :*: g) x) where
 newtype ((f :*: g) x :->: b) = PairTrie1 ((f x, g x) :->: b)
 trie f = PairTrie1 (trie (f . liftProduct))
 untrie (PairTrie1 t) = (untrie t) . dropProduct 
 enumerate (PairTrie1 t) = _enumerateWith liftProduct t

instance (HasTrie a) => HasTrie (K1 i a x) where
 data (K1 i a x :->: b) = K1Trie (a :->: b) 
 trie f = K1Trie (trie (f . K1)) 
 untrie (K1Trie t) = \(K1 a) -> (untrie t) a 
 enumerate (K1Trie t) = _enumerateWith K1 t 

instance (HasTrie (f x)) => HasTrie (M1 i t f x) where
 data (M1 i t f x :->: b) = M1Trie (f x :->: b) 
 trie f = M1Trie (trie (f . M1)) 
 untrie (M1Trie t) = \(M1 a) -> (untrie t) a  
 enumerate (M1Trie t) = _enumerateWith M1 t 


trieGeneric
 :: (Generic a, HasTrie (Reg a))
 => ((Reg a :->: b) -> (a :->: b))
 -> (a -> b)
 -> (a :->: b)
trieGeneric theConstructor f = theConstructor (trie (f . to))

untrieGeneric
 :: (Generic a, HasTrie (Reg a))
 => ((a :->: b) -> (Reg a :->: b))
 -> (a :->: b)
 -> (a -> b)
untrieGeneric theDestructor t = \a -> (untrie (theDestructor t)) (from a)

enumerateGeneric 
 :: (Generic a, HasTrie (Reg a))
 => ((a :->: b) -> (Reg a :->: b))
 -> (a :->: b)
 -> [(a, b)]
enumerateGeneric theDestructor t = _enumerateWith to (theDestructor t) 

_enumerateWith :: (HasTrie a) => (a -> a') -> (a :->: b) -> [(a', b)]
_enumerateWith f = (fmap.first) f . enumerate

dropProduct :: (f :*: g) a -> (f a, g a) 
dropProduct (a :*: b) = (a, b)

liftProduct :: (f a, g a) -> (f :*: g) a 
liftProduct (a, b) = (a :*: b)

dropSum :: (f :+: g) a -> Either (f a) (g a) 
dropSum = \case
 L1 x -> Left x 
 R1 x -> Right x 

liftSum :: Either (f a) (g a) -> (f :+: g) a 
liftSum = either L1 R1


data MyContext
 = GlobalContext  
 | EmacsContext
 | ChromeContext
 deriving (Generic) 

instance HasTrie MyContext where
 newtype (MyContext :->: b) = MyContextTrie { unMyContextTrie :: Rep MyContext () :->: b } 
 trie = trieGeneric MyContextTrie 
 untrie = untrieGeneric unMyContextTrie
 enumerate = enumerateGeneric unMyContextTrie

{-| 

e.g. 

@
makeHasTrie ''Example 
@


import Language.Haskell.TH 

makeHasTrie :: Name -> Q [Dec]
makeHasTrie t = do 
 [d| instance HasTrie $theName where
       newtype ($theName :->: b) = $theConstructor { $theDestructor :: Rep $theName () :->: b } 
       trie = trieGeneric $theConstructor 
       untrie = untrieGeneric $theDestructor |] 

 where 
 s = nameBase t 
 theName = return$ t 
 theConstructor = return$ mkName t 
 theDestructor = return$ mkName ("un" ++ t)

-} 
