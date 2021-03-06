{-# LANGUAGE CPP #-}

--------------------------------------------------

{-# LANGUAGE TypeOperators, PolyKinds, DataKinds #-}

--------------------------------------------------
--------------------------------------------------

{-| 

-}

module Enumerate.Compatibility
  ( Type
  ) where

#include <sboo-base-feature-macros.h>

--------------------------------------------------
-- Imports: CPP ----------------------------------
--------------------------------------------------

#if HAS_BASE_Type
import Data.Kind (Type)
#endif

--------------------------------------------------
-- Types -----------------------------------------
--------------------------------------------------

#if !HAS_BASE_Type
-- type Type = (*)
#endif

--------------------------------------------------
--------------------------------------------------