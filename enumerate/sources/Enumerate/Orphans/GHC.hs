{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}

{-|

-}
module Enumerate.Orphans.GHC where
import Enumerate.Types
import Enumerate.Extra

import System.Posix.Types (CIno,CMode)
import GHC.Exts(Down(..),SpecConstrAnnotation(..))
--
-- TODO CCc
-- import GHC.Conc.Windows (ConsoleEvent) -- platform-specific module
import GHC.IO.Buffer (BufferState(..))
import GHC.IO.Device (IODeviceType(..))
import GHC.IO.Encoding.Failure (CodingFailureMode(..))
import GHC.IO.Encoding.Types (CodingProgress(..))
import GHC.RTS.Flags (DoTrace,DoHeapProfile,DoCostCentres,GiveGCStats)

instance Enumerable CIno where
 enumerated = boundedEnumerated
 cardinality = boundedCardinality
instance Enumerable CMode where
 enumerated = boundedEnumerated
 cardinality = boundedCardinality

instance (Enumerable a) => Enumerable (Down a) where
  enumerated = Down <$> enumerated

instance Enumerable SpecConstrAnnotation where
 enumerated = [NoSpecConstr,ForceSpecConstr]

-- instance Enumerable ConsoleEvent where
--  enumerated = enumEnumerated

instance Enumerable BufferState where
 enumerated = [ReadBuffer,WriteBuffer]

instance Enumerable IODeviceType where
  enumerated = [Directory,Stream,RegularFile,RawDevice]

instance Enumerable CodingFailureMode where
 enumerated = [ErrorOnCodingFailure,IgnoreCodingFailure,TransliterateCodingFailure,RoundtripFailure]

instance Enumerable CodingProgress where
  enumerated = [InputUnderflow,OutputUnderflow,InvalidSequence]

instance Enumerable DoTrace where
  enumerated = enumEnumerated
instance Enumerable DoHeapProfile where
  enumerated = enumEnumerated
instance Enumerable DoCostCentres where
  enumerated = enumEnumerated
instance Enumerable GiveGCStats where
  enumerated = enumEnumerated
