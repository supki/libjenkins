{ mkDerivation, aeson, base, bytestring, directory, envparse
, filepath, lens, lens-aeson, libjenkins, optparse-applicative
, process, stdenv, text, transformers, xml-conduit
}:
mkDerivation {
  pname = "libjenkins-examples";
  version = "0.1.0.0";
  src = ./example;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson base bytestring directory envparse filepath lens lens-aeson
    libjenkins optparse-applicative process text transformers
    xml-conduit
  ];
  description = "libjenkins library examples";
  license = stdenv.lib.licenses.bsd2;
}
