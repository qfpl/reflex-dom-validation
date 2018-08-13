{ nixpkgs ? import <nixpkgs> {}
, compiler ? "ghc"
} : 
let
  inherit (nixpkgs) pkgs;
  reflex-platform = import ./nix/reflex-platform.nix;
  drv = import ./. { inherit reflex-platform compiler; };
  drvWithTools = pkgs.haskell.lib.addBuildDepends drv [
    reflex-platform.nixpkgs.pkgs.cabal-install
    reflex-platform.ghc.ghcid
  ];
in
  drvWithTools.env
