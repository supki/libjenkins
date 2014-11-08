{ haskellPackages ? (import <nixpkgs> {}).haskellPackages }:

haskellPackages.cabal.mkDerivation (self: rec {
  pname = "libjenkins";
  version = "0.6.0";
  src = ./.;
  buildDepends = with haskellPackages; [
    attoparsec free httpClient httpClientTls httpTypes liftedAsync
    liftedBase monadControl mtl network networkUri profunctors text
    transformers
  ];
  testDepends = with haskellPackages; buildDepends ++ [
    async doctest filepath hspec hspecExpectationsLens lens xmlConduit
  ];
  meta = {
    description = "Jenkins API interface";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
