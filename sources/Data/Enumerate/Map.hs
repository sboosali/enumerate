{-# LANGUAGE RankNTypes, LambdaCase #-}
{-| converting between partial functions and maps.  

>>> :set +m

-}
module Data.Enumerate.Map where
import Data.Enumerate.Types
import Data.Enumerate.Reify 

import Control.Monad.Catch (MonadThrow(..))

import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe (fromJust) 


{- | convert a total function to a map. 

@
>>> fromFunction 'not' 
fromList [(False,True),(True,False)]
@

-}
fromFunction :: (Enumerable a, Ord a) => (a -> b) -> Map a b
fromFunction f = fromFunctionM (return.f) 
{-# INLINABLE fromFunction #-}

{- | convert a (safely-)partial function to a map. 

-}
fromFunctionM :: (Enumerable a, Ord a) => (forall m. MonadThrow m => a -> m b) -> Map a b
fromFunctionM f = Map.fromList (reifyFunctionM f)
{-# INLINABLE fromFunctionM #-}

{- | convert a map to a function, if the map is total. 

@
>>> let Just not' = toFunction (Map.fromList [(False,True),(True,False)])
>>> not' False 
True 
@

-} 
toFunction :: (Enumerable a, Ord a) => Map a b -> Maybe (a -> b)
toFunction m = if isMapTotal m then Just f else Nothing 
 where f x = fromJust (Map.lookup x m) -- won't fail
{-# INLINABLE toFunction #-}

{-| does the map contain every key in its domain? 

>>> isMapTotal (Map.fromList [(False,True),(True,False)]) 
True 

>>> isMapTotal (Map.fromList [('a',0)]) 
False 

-}
isMapTotal :: (Enumerable a, Ord a) => Map a b -> Bool
isMapTotal m = all (\x -> Map.member x m) enumerated 

