{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, containers, HUnit, mtl, parsec, split
      , stdenv, test-framework, test-framework-hunit, transformers
      , ghc-mod
      }:
      mkDerivation {
        pname = "typeical";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        buildDepends = [ ghc-mod ];
        executableHaskellDepends = [
          base containers mtl parsec split transformers
        ];
        testHaskellDepends = [
          base HUnit test-framework test-framework-hunit
        ];
        description = "A humane way to write types";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
