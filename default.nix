{ ps ? (import <nixpkgs> {}).pkgs, mkDerivation
, array, base, containers, deepseq, doctest, ghc-prim
, spiros, stdenv, template-haskell, vinyl
}:
mkDerivation {
  pname = "enumerate";
  version = "0.2.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base containers deepseq ghc-prim spiros template-haskell
    vinyl
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base doctest ];
  homepage = "https://github.com/sboosali/enumerate";
  description = "enumerate all the values in a finite type (automatically)";
  license = stdenv.lib.licenses.mit;
  # shellHook = '' '';
  # buildTools = (with ps; [cabal-install haskellPackages.stack]);  
  enableSplitObjs = false;
}

