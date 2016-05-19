{-# LANGUAGE LambdaCase, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-

-}
module Enumerate.Example where
import Enumerate
import Enumerate.Extra
import Spiros.Prelude

import Data.Array (Array)
import Data.Map (Map)
import Data.Set (Set)
import Data.Ord (Ordering)

--import           System.Environment             (getArgs)
import           Data.Void (Void)
import           GHC.Generics (Generic)


-- main = mainWith =<< getArgs
--
-- mainWith = \case
--  _ -> do

main = do
    -- putStrLn ""
    -- traverse print demoEnumerated
    --
    -- putStrLn ""
    -- print $ (minBound :: Demo Bool)
    -- print $ (maxBound :: Demo Bool)
    --
    -- putStrLn ""
    -- print $ demoEnumerated == [minBound..maxBound]

    putStrLn "\n\n-- A Void"
    putStrLn ">>> cardinality ([]::[A Void])"
    print $ cardinality ([]::[A Void])
    putStrLn ">>> enumerated :: [A Void]"
    traverse print (enumerated :: [A Void])

    putStrLn "\n\n-- A ()"
    putStrLn ">>> cardinality ([]::[A ()])"
    print $ cardinality ([]::[A ()])
    putStrLn ">>> enumerated :: [A ()]"
    traverse print (enumerated :: [A ()])

    putStrLn "\n\n-- A Bool"
    putStrLn ">>> cardinality ([]::[A Bool])"
    print $ cardinality ([]::[A Bool])
    putStrLn ">>> enumerated :: [A Bool]"
    traverse print (enumerated :: [A Bool])

    putStrLn "\n\n-- A Ordering"
    putStrLn ">>> cardinality ([]::[A Ordering])"
    print $ cardinality ([]::[A Ordering])
    putStrLn ">>> enumerated :: [A Ordering]"
    traverse print (enumerated :: [A Ordering])

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

data A a
   = A0 a
   | A1 (Maybe a) (Either a a)
   | A2 (a, a)
   | A3 (Set a)
  deriving (Show,Generic,Enumerable)

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
