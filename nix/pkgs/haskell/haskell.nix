############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ haskell-nix
, gitignore-nix
, compiler-nix-name
}:

let
  project = haskell-nix.project {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = haskell-nix.haskellLib.cleanGit {
      name = "plutus-crowdfunding";
      src = ../../../.;
    };

    inherit compiler-nix-name;

    sha256map = {
      "https://github.com/Quid2/flat.git"."95e5d7488451e43062ca84d5376b3adcc465f1cd" = "06l31x3y93rjpryvlxnpsyq2zyxvb0z6lik6yq2fvh36i5zwvwa3";
      "https://github.com/input-output-hk/plutus.git"."9a2be08be986ab727c6fc400eccd92ff1c240878" = "09y1a5glvk5g25l99jamgcdw2v7y6al4pwdqapfyncl3wlsapj8f";
      "https://github.com/shmish111/purescript-bridge.git"."6a92d7853ea514be8b70bab5e72077bf5a510596" = "13j64vv116in3c204qsl1v0ajphac9fqvsjp7x3zzfr7n7g61drb";
      "https://github.com/shmish111/servant-purescript.git"."a76104490499aa72d40c2790d10e9383e0dbde63" = "11nxxmi5bw66va7psvrgrw7b7n85fvqgfp58yva99w3v9q3a50v9";
      "https://github.com/input-output-hk/cardano-crypto.git"."ce8f1934e4b6252084710975bd9bbc0a4648ece4" = "1v2laq04piyj511b2m77hxjh9l1yd6k9kc7g6bjala4w3zdwa4ni";
      "https://github.com/input-output-hk/cardano-base"."a715c7f420770b70bbe95ca51d3dec83866cb1bd" = "06l06mmb8cd4q37bnvfpgx1c5zgsl4xaf106dqva98738i8asj7j";
      "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" = "02jddik1yw0222wd6q0vv10f7y8rdgrlqaiy83ph002f9kjx7mh6";
      "https://github.com/input-output-hk/ouroboros-network"."e338f2cf8e1078fbda9555dd2b169c6737ef6774" = "12x81hpjyw2cpkazfalz6bw2wgr6ax7bnmlxl2rlfakkvsjfgaqd";
      "https://github.com/input-output-hk/iohk-monitoring-framework"."34abfb7f4f5610cabb45396e0496472446a0b2ca" = "1fdc0a02ipa385dnwa6r6jyc8jlg537i12hflfglkhjs2b7i92gs";
      "https://github.com/input-output-hk/cardano-ledger-specs"."a61e56e9405e2c2ad60969cacef0e9140bb22b94" = "0dgxl8rzjwj9j4zhf82zkbs1k8dbcdi8m515pymqqfaf1nby5raa";
      "https://github.com/input-output-hk/cardano-node.git"."f72c6272eb58a43cd1d2ef4f4381f3822b3c60c6" = "13q312z7q4avnkk0i5jl9g4idskdz3azq9xgm447ld8mwcvchdha";
      "https://github.com/input-output-hk/Win32-network"."94153b676617f8f33abe8d8182c37377d2784bd1" = "0pb7bg0936fldaa5r08nqbxvi2g8pcy4w3c7kdcg7pdgmimr30ss";
      "https://github.com/input-output-hk/hedgehog-extras"."8bcd3c9dc22cc44f9fcfe161f4638a384fc7a187" = "12viwpahjdfvlqpnzdgjp40nw31rvyznnab1hml9afpaxd6ixh70";
      "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";
    };

    modules = [
      {
        packages = {
          eventful-sql-common = {
            # This is needed so evenful-sql-common will build with a newer version of persistent.
            ghcOptions = [ "-XDerivingStrategies -XStandaloneDeriving -XUndecidableInstances -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses" ];
            doHaddock = false;
          };

          # Broken due to haddock errors. Refer to https://github.com/input-output-hk/plutus/blob/master/nix/pkgs/haskell/haskell.nix
          plutus-ledger.doHaddock = false;
          plutus-use-cases.doHaddock = false;
        };
      }
    ];
  };
in
project
