{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

let
  libjenkins = (import <nixpkgs> {})
    .stdenv.lib.overrideDerivation (import ../. {}) (_: { doCheck = false; } );
in

haskellPackages.cabal.mkDerivation (self: {
  pname = "libjenkins-examples";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = with haskellPackages; [ envParse lens lensAeson libjenkins text ];
  meta = {
    description = "libjenkins library examples";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
