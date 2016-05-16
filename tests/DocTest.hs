{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-

(the

>>> print "Data.Enumerate._..."

are for debugging.)

-}
import Test.DocTest
-- import Data.Enumerate.Extra

-- import Cabal.Info (getLibraryModules)
--
-- doctestLibraryModules = do
--   ms <- getLibraryModules >>= either (show >>> error) return
--   traverse_ print ms
--   doctest ms


main = do
 -- doctestLibraryModules

 doctest
  [ "sources/Enumerate.hs"
  , "sources/Enumerate/Types.hs"
  , "sources/Enumerate/Extra.hs"
  ]

 doctest
  [ "sources/Enumerate/Example.hs"
  ]

 doctest
   [ "sources/Enumerate/Cardinality.hs"
   ]
