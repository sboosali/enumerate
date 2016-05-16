module Enumerate.Function.Extra
 ( module Enumerate.Function.Extra

 , module Control.DeepSeq
 , module Data.Semigroup

 , module GHC.Generics
 , module Data.Data
 , module Control.Arrow

 , module Data.Function
 ) where

import Control.DeepSeq (NFData)
import Data.Semigroup (Semigroup)

import GHC.Generics (Generic)
import Data.Data (Data)
import Control.Arrow ((>>>))

import Data.Function ((&))


nothing :: (Monad m) => m ()
nothing = return ()

maybe2bool :: Maybe a -> Bool
maybe2bool = maybe False (const True)

either2maybe :: Either e a -> Maybe a
either2maybe = either (const Nothing) Just

either2bool :: Either e a -> Bool
either2bool = either (const False) (const True)
