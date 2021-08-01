let
  packages = import ./.;
  inherit (packages) pkgs plutus-crowdfunding;
  inherit (plutus-crowdfunding) haskell;

in
haskell.project.shellFor {
  withHoogle = false;

  nativeBuildInputs = with plutus-crowdfunding; [
    hlint
    cabal-install
    haskell-language-server
    stylish-haskell
    pkgs.niv
    cardano-repo-tool
  ];
}
