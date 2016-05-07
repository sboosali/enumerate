{-# LANGUAGE LambdaCase, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
module Data.Enumerate.Example where
import Data.Enumerate
import Data.Enumerate.Extra

import Data.Array (Array)
import Data.Map (Map)

--import           System.Environment             (getArgs)
import           Data.Void (Void)
import           GHC.Generics (Generic)


-- main = mainWith =<< getArgs
--
-- mainWith = \case
--  _ -> do

main = do
    putStrLn ""
    traverse print demoEnumerated

    putStrLn ""
    print $ (minBound :: Demo Bool)
    print $ (maxBound :: Demo Bool)

    putStrLn ""
    print $ demoEnumerated == [minBound..maxBound]

{- | (for documentation)

demonstrates: empty type, unit type, product type, sum type, type variable.

with @\{\-\# LANGUAGE DeriveGeneric, DeriveAnyClass \#\-\}@, the derivation is a one-liner:

@
data Demo a = ... deriving (Show,Generic,Enumerable)
@

-}
data Demo a
 = Demo0 Void
 | Demo1
 | Demo2 Bool (Maybe Bool)
 | Demo3 a
 deriving (Show,Eq,Ord,Generic,Enumerable)

{- | (for documentation)

@demoEnumerated = enumerated@

>>> traverse_ print demoEnumerated
Demo1
Demo2 False Nothing
Demo2 False (Just False)
Demo2 False (Just True)
Demo2 True Nothing
Demo2 True (Just False)
Demo2 True (Just True)
Demo3 False
Demo3 True

-}
demoEnumerated :: [Demo Bool]
demoEnumerated = enumerated

instance Bounded (Demo Bool) where
 minBound = minBound_enumerable array_DemoBool
 maxBound = maxBound_enumerable array_DemoBool

instance Enum (Demo Bool) where
 toEnum   = toEnum_enumerable   array_DemoBool
 fromEnum = fromEnum_enumerable table_DemoBool

-- CAF
array_DemoBool :: Array Int (Demo Bool)
array_DemoBool = array_enumerable

-- CAF
table_DemoBool :: Map (Demo Bool) Int
table_DemoBool = table_enumerable
