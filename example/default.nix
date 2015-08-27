{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7102" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./libjenkins-examples.nix {
  libjenkins = import ../. { inherit nixpkgs compiler; };
}
