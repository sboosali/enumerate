{-# LANGUAGE LambdaCase, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{- |



## Bounded Enum instance

You can (semi-)automatically derive efficient 'Bounded'/'Enum' 
instances:

@
instance Bounded (Demo Bool) where
 minBound = 'minBound_enumerable' array_DemoBool
 maxBound = 'maxBound_enumerable' array_DemoBool

instance Enum (Demo Bool) where
 toEnum   = 'toEnum_enumerable'   array_DemoBool
 fromEnum = 'fromEnum_enumerable' table_DemoBool

-- CAF
array_DemoBool :: 'Array' Int (Demo Bool)
array_DemoBool = 'array_enumerable'

-- CAF
table_DemoBool :: 'Map' (Demo Bool) Int
table_DemoBool = 'table_enumerable'
@

## Run

@
stack build && stack exec -- enumerable-example
@

outputs:

@
-- A Void
>>> cardinality ([]::[A Void])
1
>>> _ <- traverse print (enumerated :: [A Void])
A3 (fromList [])


-- A ()
>>> cardinality ([]::[A ()])
8
>>> _ <- traverse print (enumerated :: [A ()])
A0 ()
A1 Nothing (Left ())
A1 Nothing (Right ())
A1 (Just ()) (Left ())
A1 (Just ()) (Right ())
A2 ((),())
A3 (fromList [])
A3 (fromList [()])


-- A Bool
>>> cardinality ([]::[A Bool])
22
>>> _ <- traverse print (enumerated :: [A Bool])
A0 False
A0 True
A1 Nothing (Left False)
A1 Nothing (Left True)
A1 Nothing (Right False)
A1 Nothing (Right True)
A1 (Just False) (Left False)
A1 (Just False) (Left True)
A1 (Just False) (Right False)
A1 (Just False) (Right True)
A1 (Just True) (Left False)
A1 (Just True) (Left True)
A1 (Just True) (Right False)
A1 (Just True) (Right True)
A2 (False,False)
A2 (False,True)
A2 (True,False)
A2 (True,True)
A3 (fromList [])
A3 (fromList [False])
A3 (fromList [False,True])
A3 (fromList [True])


-- A Ordering
>>> cardinality ([]::[A Ordering])
44
>>>_ <- traverse print (enumerated :: [A Ordering])
A0 LT
A0 EQ
A0 GT
A1 Nothing (Left LT)
A1 Nothing (Left EQ)
A1 Nothing (Left GT)
A1 Nothing (Right LT)
A1 Nothing (Right EQ)
A1 Nothing (Right GT)
A1 (Just LT) (Left LT)
A1 (Just LT) (Left EQ)
A1 (Just LT) (Left GT)
A1 (Just LT) (Right LT)
A1 (Just LT) (Right EQ)
A1 (Just LT) (Right GT)
A1 (Just EQ) (Left LT)
A1 (Just EQ) (Left EQ)
A1 (Just EQ) (Left GT)
A1 (Just EQ) (Right LT)
A1 (Just EQ) (Right EQ)
A1 (Just EQ) (Right GT)
A1 (Just GT) (Left LT)
A1 (Just GT) (Left EQ)
A1 (Just GT) (Left GT)
A1 (Just GT) (Right LT)
A1 (Just GT) (Right EQ)
A1 (Just GT) (Right GT)
A2 (LT,LT)
A2 (LT,EQ)
A2 (LT,GT)
A2 (EQ,LT)
A2 (EQ,EQ)
A2 (EQ,GT)
A2 (GT,LT)
A2 (GT,EQ)
A2 (GT,GT)
A3 (fromList [])
A3 (fromList [LT])
A3 (fromList [LT,EQ])
A3 (fromList [LT,EQ,GT])
A3 (fromList [LT,GT])
A3 (fromList [EQ])
A3 (fromList [EQ,GT])
A3 (fromList [GT])

@

-}
module Enumerate.Example where
import Enumerate
import Prelude

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

    putStrLn "\nreifyCardinality @Bool..."
    print $ reifyCardinality [False]
    putStrLn "\n"

    putStrLn "\n\n-- A Void"
    putStrLn ">>> cardinality ([]::[A Void])"
    print $ cardinality ([]::[A Void])
    putStrLn ">>> enumerated :: [A Void]"
    _ <- traverse print (enumerated :: [A Void])

    putStrLn "\n\n-- A ()"
    putStrLn ">>> cardinality ([]::[A ()])"
    print $ cardinality ([]::[A ()])
    putStrLn ">>> enumerated :: [A ()]"
    _ <- traverse print (enumerated :: [A ()])

    putStrLn "\n\n-- A Bool"
    putStrLn ">>> cardinality ([]::[A Bool])"
    print $ cardinality ([]::[A Bool])
    putStrLn ">>> enumerated :: [A Bool]"
    _ <- traverse print (enumerated :: [A Bool])

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

>>> _ <- traverse print demoEnumerated
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
 minBound = minBound_Enumerable array_DemoBool
 maxBound = maxBound_Enumerable array_DemoBool

instance Enum (Demo Bool) where
 toEnum   = toEnum_Enumerable   array_DemoBool
 fromEnum = fromEnum_Enumerable table_DemoBool

-- CAF
array_DemoBool :: Array Int (Demo Bool)
array_DemoBool = array_Enumerable

-- CAF
table_DemoBool :: Map (Demo Bool) Int
table_DemoBool = table_Enumerable

-------------------------------------------------------
