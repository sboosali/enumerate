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
  [ "sources/Data/Enumerate.hs"
  , "sources/Data/Enumerate/Types.hs"
  , "sources/Data/Enumerate/Extra.hs"
  ]

 doctest
  [ "sources/Data/Enumerate/Function.hs"
  , "sources/Data/Enumerate/Example.hs"
  ]

 doctest
   [ "sources/Data/Cardinality.hs"
   ]

   -- split up because some modules only succeed when they have the interpreter to themselves.
   -- seems like there's incompatibility between the Data.Enumerate.Types a module is *built* with,
   -- and they types that are present when it is *interpreted*.

 --
 -- doctest
 --  [ "sources/Data/Enumerate/Reify.hs" -- freezes
 --  ]
 --
 --  doctest
 --   [ "sources/Data/Enumerate/Map.hs" -- freezes
 --   ]
