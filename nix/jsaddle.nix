let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    jsaddle-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./jsaddle.json;
      inherit (jsaddle-info-pinned) rev sha256;
    jsaddle = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "ghcjs";
      repo = "jsaddle";
      inherit (jsaddle-info-pinned) rev sha256;
    };
  };

in
  sources.jsaddle

