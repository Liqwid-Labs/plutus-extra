{ sourcesFile ? ./sources.json, system ? builtins.currentSystem
, sources ? import ./sources.nix { inherit system sourcesFile; }
, plutus ? import sources.plutus { }, deferPluginErrors ? true
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
      eventful-sql-common.ghcOptions = [''
        -XDerivingStrategies -XStandaloneDeriving -XUndecidableInstances
                -XDataKinds -XFlexibleInstances -XMultiParamTypeClasses''];
      marlowe.doHaddock = deferPluginErrors;
      marlowe.flags.defer-plugin-errors = deferPluginErrors;

      plutus-use-cases.doHaddock = deferPluginErrors;
      plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;

      plutus-ledger.doHaddock = deferPluginErrors;
      plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;

      cardano-crypto-praos.components.library.pkgconfig =
        pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];
      cardano-crypto-class.components.library.pkgconfig =
        pkgs.lib.mkForce [ [ pkgs.libsodium-vrf ] ];

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
    "https://github.com/input-output-hk/plutus.git"."${sources.plutus.rev}" =
      sources.plutus.sha256;

    # michaelpj/flat
    "https://github.com/michaelpj/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" =
      "1lrzknw765pz2j97nvv9ip3l1mcpf2zr4n56hwlz0rk7wq7ls4cm";

    # shmish111/purescript-bridge
    "https://github.com/shmish111/purescript-bridge.git"."6a92d7853ea514be8b70bab5e72077bf5a510596" =
      "13j64vv116in3c204qsl1v0ajphac9fqvsjp7x3zzfr7n7g61drb";

    # shmish111/servant-purescript
    "https://github.com/shmish111/servant-purescript.git"."a76104490499aa72d40c2790d10e9383e0dbde63" =
      "11nxxmi5bw66va7psvrgrw7b7n85fvqgfp58yva99w3v9q3a50v9";

    # input-output-hk/cardano-base
    "https://github.com/input-output-hk/cardano-base"."cb0f19c85e5bb5299839ad4ed66af6fa61322cc4" =
      "0dnkfqcvbifbk3m5pg8kyjqjy0zj1l4vd23p39n6ym4q0bnib1cq";

    # input-output-hk/cardano-crypto
    "https://github.com/input-output-hk/cardano-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" =
      "06sdx5ndn2g722jhpicmg96vsrys89fl81k8290b3lr6b1b0w4m3";

    # input-output-hk/cardano-ledger-specs
    "https://github.com/input-output-hk/cardano-ledger-specs"."12a0ef69d64a55e737fbf4e846bd8ed9fb30a956" =
      "0mx1g18ypdd5m8ijc2cl9m1xmymlqfbwl1r362f92vxrmziacifv";

    # input-output-hk/cardano-prelude
    "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" =
      "02jddik1yw0222wd6q0vv10f7y8rdgrlqaiy83ph002f9kjx7mh6";

    # input-output-hk/goblins
    "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" =
      "17c88rbva3iw82yg9srlxjv2ia5wjb9cyqw44hik565f5v9svnyg";

    # input-output-hk/iohk-monitoring-framework
    "https://github.com/input-output-hk/iohk-monitoring-framework"."34abfb7f4f5610cabb45396e0496472446a0b2ca" =
      "1fdc0a02ipa385dnwa6r6jyc8jlg537i12hflfglkhjs2b7i92gs";

    # input-output-hk/ouroboros-network
    "https://github.com/input-output-hk/ouroboros-network"."f149c1c1e4e4bb5bab51fa055e9e3a7084ddc30e" =
      "1szh3xr7qnx56kyxd554yswpddbavb7m7k2mk3dqdn7xbg7s8b8w";

    # input-output-hk/optparse-applicative
    "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" =
      "1gvsrg925vynwgqwplgjmp53vj953qyh3wbdf34pw21c8r47w35r";

    # input-output-hk/cardano-node
    "https://github.com/input-output-hk/cardano-node.git"."3a56ac245c83d3345f81123ec3bb496bb23477a3" =
      "0dglxqhqrdn5nc3n6c8b7himgxrjdjszcl905xihrnaav49z09mg";

    # input-output-hk/Win32-network
    "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" =
      "19wahfv726fa3mqajpqdqhnl9ica3xmf68i254q45iyjcpj1psqx";

    # input-output-hk/hedgehog-extras
    "https://github.com/input-output-hk/hedgehog-extras"."edf6945007177a638fbeb8802397f3a6f4e47c14" =
      "0wc7qzkc7j4ns2rz562h6qrx2f8xyq7yjcb7zidnj7f6j0pcd0i9";
  };
}
