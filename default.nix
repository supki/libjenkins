{ cabal, async, attoparsec, doctest, filepath, free, hspec
, hspecExpectationsLens, httpClient, httpClientTls, httpTypes, lens
, liftedAsync, liftedBase, monadControl, mtl, network, networkUri
, profunctors, text, transformers, xmlConduit
}:

cabal.mkDerivation (self: rec {
  pname = "libjenkins";
  version = "0.6.0";
  src = ./.;
  buildDepends = [
    attoparsec free httpClient httpClientTls httpTypes liftedAsync
    liftedBase monadControl mtl network networkUri profunctors text
    transformers
  ];
  testDepends = [
    async doctest filepath hspec hspecExpectationsLens lens xmlConduit
  ];
  meta = {
    description = "Jenkins API interface";
    license = self.stdenv.lib.licenses.bsd3;
    platforms = self.ghc.meta.platforms;
  };
})
