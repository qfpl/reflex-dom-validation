{ system ? builtins.currentSystem # TODO: Get rid of this system cruft
, iosSdkVersion ? "10.2"
}:
with import ./.obelisk/impl { inherit system iosSdkVersion; };
project ./. ({ ... }: {
  overrides = self: super: {
    reflex-dom-validation = super.callCabal2nix "reflex-dom-validation" ../. {};
})
