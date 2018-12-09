{ mkDerivation, aeson, base, bytestring, colour, containers
, dependent-map, dependent-sum, errors, ghcjs-dom, jsaddle-warp
, lens, mtl, ref-tf, reflex, reflex-dom, reflex-dom-storage, stdenv
, text, time, validation, wai-app-static
}:
mkDerivation {
  pname = "reflex-dom-validation";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base bytestring colour containers dependent-map dependent-sum
    errors ghcjs-dom jsaddle-warp lens mtl ref-tf reflex reflex-dom
    reflex-dom-storage text time validation wai-app-static
  ];
  license = stdenv.lib.licenses.bsd3;
}
