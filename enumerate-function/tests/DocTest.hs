{-# LANGUAGE CPP #-}

--------------------------------------------------

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

--------------------------------------------------
--------------------------------------------------

#include <sboo-base-feature-macros.h>

--------------------------------------------------

import "doctest" Test.DocTest

--------------------------------------------------

import "base" Prelude

--------------------------------------------------
--------------------------------------------------

{-

$ grep -h -i LANGUAGE -r sources/ | sort | uniq | ...

$ cat ... | sort | uniq | xargs | sed -e 's/ / /g'

-}

-- [1] every module in this directory (i.e. `hs-source-dirs`),
-- [2] and all language extensions,
-- whether enabled by default or otherwise used,
-- (i.e. both `default-extensions` and `other-extensions`)
-- EXCEPT those that conflict
-- (e.g. DeriveAnyClass and GeneralizedNewtypeDeriving)

--------------------------------------------------
-- Main ------------------------------------------
--------------------------------------------------

main = do
  doctest (sources ++ flags)

--------------------------------------------------

sources = [  "sources/" ]

---sources = modules2filepaths "hs" "sources" $ ""

--------------------------------------------------

flags = extensions ++ options

  where

  ------------------------------

  extensions = extensions2flags $
      "CPP NoImplicitPrelude ConstraintKinds DataKinds DefaultSignatures DeriveDataTypeable DeriveGeneric ExplicitNamespaces FlexibleContexts FlexibleInstances GADTs KindSignatures LambdaCase RankNTypes ScopedTypeVariables TupleSections TypeFamilies TypeOperators UndecidableInstances PackageImports"
#if HAS_EXTENSION_DerivingStrategies
   ++ " DerivingStrategies"
#endif

  ------------------------------

  options = [
            ]

--------------------------------------------------
-- Utilities -------------------------------------
--------------------------------------------------

putStringsLine :: [String] -> IO ()
putStringsLine = fmap (const ()) . traverse putStrLn

--------------------------------------------------

printDivider :: IO ()
printDivider = putStrLn "----------------------------------------\n"

--------------------------------------------------

printBlank :: IO ()
printBlank = putStrLn ""

--------------------------------------------------

extensions2flags :: String -> [String]
extensions2flags = fmap ("-X"++) . words

--------------------------------------------------

modules2filepaths :: String -> String -> String -> [String]
modules2filepaths extension directory = fmap go . words

 where
 go s = directory ++ "/" ++ (module2filename s) ++ "." ++ extension

--------------------------------------------------

module2filename :: String -> String
module2filename = replace '.' '/'

--------------------------------------------------

replace
  :: (Functor f, Eq a)
  => a -> a -> f a -> f a

replace a b = fmap go
  where

  go c = if c == a then b else c

--------------------------------------------------
--------------------------------------------------

    --TODO: split up because some modules only succeed when they have the interpreter to themselves.
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
