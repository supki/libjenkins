let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      libjenkins = self.callPackage ./. {};
    };
  };
in pkgs.lib.overrideDerivation haskellPackages.libjenkins (attrs: {
  buildInputs = [ pkgs.haskellPackages.hdevtools pkgs.haskellPackages.cabalInstall ] ++ attrs.buildInputs;
})
