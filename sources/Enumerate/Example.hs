{-# LANGUAGE LambdaCase, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-

-}
module Enumerate.Example where
import Enumerate
import Enumerate.Extra

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

    putStrLn "\n\nA Void\n"
    putStrLn $ "|A Void| = " ++ show (cardinality ([]::[A Void]))
    putStrLn "A Void = { ... }"
    traverse print (enumerated :: [A Void])

    putStrLn "\n\nA ()\n"
    putStrLn $ "|A ()| = " ++ show (cardinality ([]::[A ()]))
    putStrLn "A () = { ... }"
    traverse print (enumerated :: [A ()])

    putStrLn "\n\nA Bool\n"
    putStrLn $ "|A Bool| = " ++ show (cardinality ([]::[A Bool]))
    putStrLn "A Bool = { ... }"
    traverse print (enumerated :: [A Bool])

    putStrLn "\n\nA Ordering\n"
    putStrLn $ "|A Ordering| = " ++ show (cardinality ([]::[A Ordering]))
    putStrLn "A Ordering = { ... }"
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
