
/*
{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = import ./default.nix;
  ps = {
    spiros = ../spiros;
  };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

#  drv = pkgs.stdenv.lib.overrideDerivation drv0 (as: is);
#  drv = drv0.overrideDerivation (as: is); 
#  drv = pkgs.haskell.lib.addBuildTool drv0 (with pkgs; [cabal-install] );
  drv = drv0;
  drv0 = haskellPackages.callPackage f ps;

  is = {
    buildTools = with pkgs; [cabal-install];
  };

in

  if pkgs.lib.inNixShell then drv.env else drv

*/


/*
with (import <nixpkgs> {}).pkgs;
let

myHaskellPackages = haskellPackages.override {
 overrides = self: super: {
   spiros    = self.callPackage ../spiros {};
   enumerate = self.callPackage ./. {};
   };
 };

 in

myHaskellPackages.enumerate.env

*/

{ nixpkgs ? import <nixpkgs> {}
, compiler ? "default" 
}:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  f = import ./package.nix;

  ps = {
   spiros = haskellPackages.callPackage ~/spiros {}; # absolute
  };

  d0 = haskellPackages.callPackage f ps;
  d = (pkgs.haskell.lib.addBuildTools d0 [
        haskellPackages.cabal-install
        haskellPackages.hlint
      ]);

  e = if pkgs.lib.inNixShell then d.env else d;

in 

e
