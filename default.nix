{ reflex-platform ? import ./nix/reflex-platform.nix
, compiler   ? "ghc"
} :
let

  pkgs = reflex-platform.nixpkgs.pkgs;
  ghc = reflex-platform.${compiler};
  haskellPackages = ghc.override {
    overrides = self: super: {
      validation = super.callCabal2nix "validation" (import ./nix/validation.nix) {};
      reflex-dom-storage = super.callCabal2nix "reflex-dom-storage" (import ./nix/reflex-dom-storage.nix) {};
      jsaddle-warp = pkgs.haskell.lib.dontCheck (super.callCabal2nix "jsaddle-warp" "${import ./nix/jsaddle.nix}/jsaddle-warp" {});
    };
  };
  drv = haskellPackages.callPackage ./reflex-dom-validation.nix {};
in
  drv
