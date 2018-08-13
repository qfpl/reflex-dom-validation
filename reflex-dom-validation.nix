{ mkDerivation, aeson, base, containers, dependent-map
, dependent-sum, ghcjs-dom, jsaddle-warp, lens, mtl, reflex
, reflex-dom, reflex-dom-storage, stdenv, text, validation
, wai-app-static
}:
mkDerivation {
  pname = "reflex-dom-validation";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson base containers dependent-map dependent-sum ghcjs-dom
    jsaddle-warp lens mtl reflex reflex-dom reflex-dom-storage text
    validation wai-app-static
  ];
  license = stdenv.lib.licenses.bsd3;
}
