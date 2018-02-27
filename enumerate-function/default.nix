{ mkDerivation, base, containers, criterion, deepseq, doctest
, enumerate, exceptions, hspec, MemoTrie, QuickCheck, semigroups
, stdenv
}:
mkDerivation {
  pname = "enumerate-function";
  version = "0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers deepseq enumerate exceptions MemoTrie semigroups
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base doctest hspec QuickCheck ];
  benchmarkHaskellDepends = [ base criterion deepseq ];
  homepage = "http://github.com/sboosali/enumerate-function#readme";
  description = "simple package for inverting functions and testing totality, via brute enumeration of the domain";
  license = stdenv.lib.licenses.bsd3;
}
