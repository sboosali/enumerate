{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-

(the

>>> print "Data.Enumerate._..."

are for debugging.)

-}
import Test.DocTest

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

extensions2flags :: String -> [String]
extensions2flags = fmap ("-X"++) . words

main = doctest $
 [ "sources/" ] ++ extensions2flags "ConstraintKinds DataKinds DefaultSignatures DeriveDataTypeable DeriveGeneric ExplicitNamespaces FlexibleContexts FlexibleInstances GADTs KindSignatures LambdaCase RankNTypes ScopedTypeVariables TupleSections TypeFamilies TypeOperators UndecidableInstances"

