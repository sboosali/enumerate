{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
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
  , "sources/Data/Enumerate/Example.hs"
  , "sources/Data/Enumerate/Large.hs"
  , "sources/Data/Enumerate/Enum.hs"
  , "sources/Data/Enumerate/Reify.hs"
  , "sources/Data/Enumerate/Function.hs"
  , "sources/Data/Enumerate/Map.hs"
  ]
