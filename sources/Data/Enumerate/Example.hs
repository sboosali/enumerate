{-# LANGUAGE LambdaCase, DeriveGeneric, DeriveAnyClass #-}
module Data.Enumerate.Example where 
import Data.Enumerate 

import           System.Environment             (getArgs)
import           Data.Void (Void)
import           GHC.Generics (Generic) 


main = mainWith =<< getArgs

mainWith = \case
 _ -> return() 


{- | (for documentation) 

needs @\{\-\# LANGUAGE DeriveGeneric, DeriveAnyClass \#\-\}@

demonstrates: empty type, unit type, product type, sum type, type variable.

-}
data DemoEnumerable a
 = DemoEnumerable0 Void
 | DemoEnumerable1
 | DemoEnumerable2 Bool (Maybe Bool) 
 | DemoEnumerable3 a
 deriving (Show,Generic,Enumerable) 

{- | (for documentation) 

@demoEnumerated = enumerated@

>>> demoEnumerated
[DemoEnumerable1,DemoEnumerable2 False Nothing,DemoEnumerable2 False (Just False),DemoEnumerable2 False (Just True),DemoEnumerable2 True Nothing,DemoEnumerable2 True (Just False),DemoEnumerable2 True (Just True),DemoEnumerable3 False,DemoEnumerable3 True]

-}
demoEnumerated :: [DemoEnumerable Bool] 
demoEnumerated = enumerated

