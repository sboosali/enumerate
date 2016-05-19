{-# LANGUAGE CPP, PackageImports, NoImplicitPrelude #-}

module Spiros.Prelude
( module Prelude_
, traverse_
#if MIN_VERSION_base(4,8,0)
#else
  (<$>),
  Monoid(..),
  Applicative(..),
#endif
)
where

#if MIN_VERSION_base(4,6,0)
import "base" Prelude as Prelude_
#else
import "base" Prelude as Prelude_ hiding (catch)
#endif

#if MIN_VERSION_base(4,8,0)
#else
import Data.Functor((<$>))
import Data.Monoid(Monoid(..))
import Control.Applicative(Applicative(..))
#endif

import Data.Foldable  (traverse_)
