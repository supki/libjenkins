let
  pkgs = import <nixpkgs> {};
  env  = pkgs.haskellPackages.ghcWithPackagesOld (ghc:
    ([ ghc.hlint ghc.doctest ] ++ [(import ../../hdevtools {})] ++ (import ./. {}).propagatedNativeBuildInputs));
in
  pkgs.myEnvFun {
    name = "libjenkins-examples";
	shell = "/usr/bin/zsh";
    buildInputs = [ pkgs.haskellPackages.cabalInstall env ];
    extraCmds = ''
      $(grep export ${env.outPath}/bin/ghc)
    '';
    }
