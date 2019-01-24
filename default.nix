{ reflex-platform ? import ./nix/reflex-platform.nix
, compiler   ? "ghc"
} :
let

  pkgs = reflex-platform.nixpkgs.pkgs;
  ghc = reflex-platform.${compiler};
  haskellPackages = ghc.override {
    overrides = self: super: {
      Glob = pkgs.haskell.lib.dontCheck super.Glob;
      http-date = pkgs.haskell.lib.dontCheck super.http-date;
      iproute = pkgs.haskell.lib.dontCheck super.iproute;
      mockery = pkgs.haskell.lib.dontCheck super.mockery;
      unix-time = pkgs.haskell.lib.dontCheck super.unix-time;
      http2 = pkgs.haskell.lib.dontCheck super.http2;
      bsb-http-chunked = pkgs.haskell.lib.dontCheck super.bsb-http-chunked;
      wai-extra = pkgs.haskell.lib.dontCheck super.wai-extra;
      wai-app-static = pkgs.haskell.lib.dontCheck super.wai-app-static;
      validation = pkgs.haskell.lib.dontCheck super.validation;
      reflex-dom-storage = super.callCabal2nix "reflex-dom-storage" (import ./nix/reflex-dom-storage.nix) {};
      jsaddle-warp = pkgs.haskell.lib.dontCheck (super.callCabal2nix "jsaddle-warp" "${import ./nix/jsaddle.nix}/jsaddle-warp" {});
    };
  };
  drv = haskellPackages.callPackage ./reflex-dom-validation.nix {};
in
  drv
