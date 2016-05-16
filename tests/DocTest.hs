{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import Test.DocTest

main = do
 return()

    -- split up because some modules only succeed when they have the interpreter to themselves.
    -- seems like there's incompatibility between the Data.Enumerate.Types a module is *built* with,
    -- and they types that are present when it is *interpreted*.
 -- 
 -- doctest
 --  [
 --  ]

 -- doctest
 --  [ "sources/Enumerate/Function/Reify.hs" -- freezes
 --  ]

--  doctest
--   [ "sources/Enumerate/Map.hs" -- freezes
--   ]
