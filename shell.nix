{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  local = { 
    spiros = ../spiros;
  };

  f = { mkDerivation, array, base, containers, deepseq, doctest
      , ghc-prim, spiros, stdenv, template-haskell, vinyl
      }:
      mkDerivation {
        pname = "enumerate";
        version = "0.2.1";
        sha256 = "0";
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
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f local;

in

  if pkgs.lib.inNixShell then drv.env else drv
