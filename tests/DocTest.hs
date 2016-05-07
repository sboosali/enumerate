{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import Test.DocTest
import Data.Enumerate.Extra

import Cabal.Info (getLibraryModules)

doctestLibraryModules = do
  ms <- getLibraryModules >>= either (show >>> error) return
  traverse_ print ms
  doctest ms


main = do
 -- doctestLibraryModules

 doctest
  [ "C:\\Users\\Maria\\commands\\enumerate\\sources\\Data\\Enumerate.hs"
  , "C:\\Users\\Maria\\commands\\enumerate\\sources\\Data\\Enumerate\\Types.hs"
  , "C:\\Users\\Maria\\commands\\enumerate\\sources\\Data\\Enumerate\\Example.hs"
  , "C:\\Users\\Maria\\commands\\enumerate\\sources\\Data\\Enumerate\\Extra.hs"
  , "C:\\Users\\Maria\\commands\\enumerate\\sources\\Data\\Enumerate\\Large.hs"
  , "C:\\Users\\Maria\\commands\\enumerate\\sources\\Data\\Enumerate\\Enum.hs"
  , "C:\\Users\\Maria\\commands\\enumerate\\sources\\Data\\Enumerate\\Reify.hs"
  --, "C:\\Users\\Maria\\commands\\enumerate\\sources\\Data\\Enumerate\\Function.hs"
  --, "C:\\Users\\Maria\\commands\\enumerate\\sources\\Data\\Enumerate\\Map.hs"
  ]
