{ mkDerivation, async, attoparsec, base, bytestring, conduit
, containers, directory, doctest, filepath, free, hspec
, hspec-expectations-lens, http-client, http-conduit, http-types
, lens, lifted-async, lifted-base, monad-control, mtl, network
, network-uri, profunctors, resourcet, stdenv, text, transformers
, xml-conduit
}:
mkDerivation {
  pname = "libjenkins";
  version = "0.8.1";
  src = ./.;
  buildDepends = [
    async attoparsec base bytestring conduit containers free
    http-client http-conduit http-types monad-control mtl network
    network-uri profunctors resourcet text transformers
  ];
  testDepends = [
    async attoparsec base bytestring conduit containers directory
    doctest filepath free hspec hspec-expectations-lens http-client
    http-conduit http-types lens lifted-async lifted-base monad-control
    mtl network network-uri profunctors resourcet text transformers
    xml-conduit
  ];
  description = "Jenkins API interface";
  license = stdenv.lib.licenses.bsd2;
}
