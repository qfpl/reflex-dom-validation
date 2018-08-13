let  
  initialNixpkgs = import <nixpkgs> {};

  sources = rec {
    validation-info-pinned = initialNixpkgs.pkgs.lib.importJSON ./validation.json;
      inherit (validation-info-pinned) rev sha256;
    validation = initialNixpkgs.pkgs.fetchFromGitHub {
      owner = "qfpl";
      repo = "validation";
      inherit (validation-info-pinned) rev sha256;
    };
  };

in
  sources.validation

