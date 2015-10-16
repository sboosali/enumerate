{-# OPTIONS_GHC -fno-warn-orphans #-}
{-| orphan instances for large types. 

(that are included for completeness, but not exported by default (i.e. by "Data.Enumerate"). 
you probably want build-time instance-resolution errors rather than runtime non-termination). 

-}
module Data.Enumerate.Large where
import Data.Enumerate.Types 

import           Data.Word (Word32, Word64)
import           Data.Int (Int32, Int64)
import           Data.Proxy


{-| brute-force function extensionality. 

warning: the size of the domain grows exponentially in the number of arguments. 

-}
instance (Enumerable a, Eq b) => Eq (a -> b) where
 f == g = all ((==) <$> f <*> g) enumerated
 f /= g = any ((/=) <$> f <*> g) enumerated

instance Enumerable Int32  where enumerated = boundedEnumerated; cardinality = boundedCardinality 
instance Enumerable Word32 where enumerated = boundedEnumerated; cardinality = boundedCardinality 

-- | finite but too big. @2^64@ is over a billion billion (@1,000,000,000,000@). e.g. 'Enumerate.reifyFunction' on a function of type @(:: Int -> Bool)@, even a lazy one, won't terminate anytime soon. 
instance Enumerable Int64  where enumerated = boundedEnumerated; cardinality = boundedCardinality 
instance Enumerable Word64 where enumerated = boundedEnumerated; cardinality = boundedCardinality 

