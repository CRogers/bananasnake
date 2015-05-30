with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, base, reactive-banana, stdenv, terminal-size, cabal-install, ghcid }:
             mkDerivation {
               pname = "bananasnake";
               version = "0.1.0.0";
               src = ./.;
               isLibrary = false;
               isExecutable = true;
               buildDepends = [ base reactive-banana terminal-size cabal-install ghcid ];
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
