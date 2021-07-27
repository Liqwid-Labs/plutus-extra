{ sourcesFile ? ./sources.json
, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }
, deferPluginErrors ? true
, doCoverage ? false }:
let inherit (plutus) pkgs;
in pkgs.haskell-nix.cabalProject rec {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "plutus-extra";
    src = ./..;
  };

  # Plutus uses a patched GHC. And so shall we.
  compiler-nix-name = "ghc810420210212";

  # -- Materialization
  # See https://input-output-hk.github.io/haskell.nix/tutorials/materialization/:
  # Update using:
  #   nix-build default.nix 2>&1 | grep -om1 '/nix/store/.*-updateMaterialized' | bash
  # plan-sha256 = "0m56bhk9w3v1zqpig84f9krrp6sqg21w0vxbjiqcxz8n7c39aw54";
  # materialized = ./materialization/plutus-extra.materialized;

  modules = [{
    packages = {
      eventful-sql-common.doHaddock = false;
      eventful-sql-common.ghcOptions = [        
        "-XDerivingStrategies -XStandaloneDeriving -XUndecidableInstances
        -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses"
      ];
      marlowe.doHaddock = deferPluginErrors;
      marlowe.flags.defer-plugin-errors = deferPluginErrors;

      plutus-use-cases.doHaddock = deferPluginErrors;
      plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;

      plutus-ledger.doHaddock = deferPluginErrors;
      plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;

      # This allows us to generate .tix coverage files, which could be useful?
      "${src.name}".components.library.doCoverage = doCoverage;
    };
  }];

  # Using this allows us to leave these nix-specific hashes _out_ of cabal.project
  # Normally, they'd be placed under the `source-repository-package` section as a comment like so:
  # `--sha256: ...`
  sha256map = {
    # Enforce we are using the same hash as niv has
    # i.e. this will now fail to nix-build if you bump it but don't bump the `cabal.project`.

    # input-output-hk/plutus
    "https://github.com/input-output-hk/plutus.git"."${sources.plutus.rev}"
    = sources.plutus.sha256;

    # Quid2/flat
    "https://github.com/Quid2/flat.git"."95e5d7488451e43062ca84d5376b3adcc465f1cd"
    = "06l31x3y93rjpryvlxnpsyq2zyxvb0z6lik6yq2fvh36i5zwvwa3";

    # shmish111/purescript-bridge
    "https://github.com/shmish111/purescript-bridge.git"."6a92d7853ea514be8b70bab5e72077bf5a510596"
    = "13j64vv116in3c204qsl1v0ajphac9fqvsjp7x3zzfr7n7g61drb";

    # shmish111/servant-purescript
    "https://github.com/shmish111/servant-purescript.git"."a76104490499aa72d40c2790d10e9383e0dbde63"
    = "11nxxmi5bw66va7psvrgrw7b7n85fvqgfp58yva99w3v9q3a50v9";

    # input-output-hk/cardano-base
    "https://github.com/input-output-hk/cardano-base"."a715c7f420770b70bbe95ca51d3dec83866cb1bd"
    = "06l06mmb8cd4q37bnvfpgx1c5zgsl4xaf106dqva98738i8asj7j";

    # input-output-hk/cardano-crypto
    "https://github.com/input-output-hk/cardano-crypto.git"."ce8f1934e4b6252084710975bd9bbc0a4648ece4"
    = "1v2laq04piyj511b2m77hxjh9l1yd6k9kc7g6bjala4w3zdwa4ni";

    # input-output-hk/cardano-ledger-specs
    "https://github.com/input-output-hk/cardano-ledger-specs"."6b0fca7a73c317f3af7c14dd4dc38178cc78a6c8"
    = "0570g723ac8wf0zha37nsh4n0809rqqfx4j9i80hqkq18cysrglr";

    # input-output-hk/cardano-prelude
    "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852"
    = "02jddik1yw0222wd6q0vv10f7y8rdgrlqaiy83ph002f9kjx7mh6";

    # input-output-hk/goblins
    "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba"
    = "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";

    # input-output-hk/iohk-monitoring-framework
    "https://github.com/input-output-hk/iohk-monitoring-framework"."34abfb7f4f5610cabb45396e0496472446a0b2ca"
    = "1fdc0a02ipa385dnwa6r6jyc8jlg537i12hflfglkhjs2b7i92gs";

    # input-output-hk/ouroboros-network
    "https://github.com/input-output-hk/ouroboros-network"."e338f2cf8e1078fbda9555dd2b169c6737ef6774"
    = "12x81hpjyw2cpkazfalz6bw2wgr6ax7bnmlxl2rlfakkvsjfgaqd";

    # input-output-hk/cardano-node
    "https://github.com/input-output-hk/cardano-node.git"."f3ef4ed72894499160f2330b91572a159005c148"
    = "1mp8ih6kmq4j354mgjgrxlssv7jbk5zz1j3nyqg43ascql4d0fvq";

    # input-output-hk/Win32-network
    "https://github.com/input-output-hk/Win32-network"."94153b676617f8f33abe8d8182c37377d2784bd1"
    = "0pb7bg0936fldaa5r08nqbxvi2g8pcy4w3c7kdcg7pdgmimr30ss";

    # input-output-hk/hedgehog-extras
    "https://github.com/input-output-hk/hedgehog-extras"."8bcd3c9dc22cc44f9fcfe161f4638a384fc7a187" =
      "12viwpahjdfvlqpnzdgjp40nw31rvyznnab1hml9afpaxd6ixh70";
  };
}
