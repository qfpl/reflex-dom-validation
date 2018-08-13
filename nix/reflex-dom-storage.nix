let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    reflex-dom-storage-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./reflex-dom-storage.json;
      inherit (reflex-dom-storage-info-pinned) rev sha256;
    reflex-dom-storage = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "reflex-dom-storage";
      inherit (reflex-dom-storage-info-pinned) rev sha256;
    };
  };

in
  sources.reflex-dom-storage

