{
  description = "plutus-extra";

  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2105";
  inputs.plutus.url = "github:input-output-hk/plutus"; # used for libsodium-vrf

  outputs = { self, nixpkgs, haskell-nix, plutus }:
    let
      supportedSystems =
        [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system:
        import nixpkgs {
          inherit system;
          overlays = [ haskell-nix.overlay ];
          inherit (haskell-nix) config;
        };

      projectFor = system:
        let
          deferPluginErrors = true;
          pkgs = nixpkgsFor system;

          fakeSrc = pkgs.runCommand "real-source" { } ''
            cp -rT ${self} $out
            chmod u+w $out/cabal.project
            cat $out/cabal-haskell.nix.project >> $out/cabal.project
          '';
        in (nixpkgsFor system).haskell-nix.cabalProject' {
          src = fakeSrc.outPath;
          compiler-nix-name = "ghc8107";
          cabalProjectFileName = "cabal.project";
          modules = [{
            packages = {
              marlowe.flags.defer-plugin-errors = deferPluginErrors;
              plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;
              plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
              plutus-contract.flags.defer-plugin-errors = deferPluginErrors;
              cardano-crypto-praos.components.library.pkgconfig =
                nixpkgs.lib.mkForce
                [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
              cardano-crypto-class.components.library.pkgconfig =
                nixpkgs.lib.mkForce
                [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
            };
          }];
          shell = {
            withHoogle = true;

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs = [
              pkgs.cabal-install
              pkgs.haskellPackages.fourmolu
              pkgs.entr
              pkgs.nixfmt
              pkgs.haskellPackages.cabal-fmt
            ];

            tools = { 
              hlint = "3.3.6";
            };

            additional = ps: [
              ps.base-deriving-via
              ps.cardano-addresses
              ps.cardano-addresses-cli
              ps.cardano-binary
              ps.cardano-crypto
              ps.cardano-crypto-class
              ps.cardano-crypto-praos
              ps.cardano-crypto-wrapper
              ps.cardano-ledger-alonzo
              ps.cardano-ledger-byron
              ps.cardano-ledger-core
              ps.cardano-ledger-pretty
              ps.cardano-ledger-shelley
              ps.cardano-ledger-shelley-ma
              ps.cardano-prelude
              ps.cardano-slotting
              ps.flat
              ps.freer-extras
              ps.goblins
              ps.measures
              ps.orphans-deriving-via
              ps.playground-common
              ps.plutus-contract
              ps.plutus-core
              ps.plutus-ledger
              ps.plutus-ledger-api
              ps.plutus-pab
              ps.plutus-playground-server
              ps.plutus-tx
              ps.plutus-tx-plugin
              ps.plutus-use-cases
              ps.prettyprinter-configurable
              ps.quickcheck-dynamic
              ps.Win32-network
              ps.word-array
              ps.tasty-expected-failure
            ];
          };
          sha256map = {
            "https://github.com/input-output-hk/plutus"."cc72a56eafb02333c96f662581b57504f8f8992f" =
              "sha256-Pl3M9rMEoiEKRsTdDr4JwNnRo5Xs4uN66uVpOfaMCfE=";
            "https://github.com/input-output-hk/plutus-apps.git"."7f543e21d4945a2024e46c572303b9c1684a5832" =
              "sha256-OX4+S7fFUqXRz935wQqdcEm1I6aqg0udSdP19XJtSAk=";
            "https://github.com/Quid2/flat"."ee59880f47ab835dbd73bea0847dab7869fc20d8" =
              "lRFND+ZnZvAph6ZYkr9wl9VAx41pb3uSFP8Wc7idP9M=";
            "https://github.com/input-output-hk/purescript-bridge"."47a1f11825a0f9445e0f98792f79172efef66c00" =
              "sha256-/SbnmXrB9Y2rrPd6E79Iu5RDaKAKozIl685HQ4XdQTU=";
            "https://github.com/input-output-hk/servant-purescript"."44e7cacf109f84984cd99cd3faf185d161826963" =
              "sha256-DH9ISydu5gxvN4xBuoXVv1OhYCaqGOtzWlACdJ0H64I=";
            "https://github.com/input-output-hk/cardano-crypto"."f73079303f663e028288f9f4a9e08bcca39a923e" =
              "sha256-2Fipex/WjIRMrvx6F3hjJoAeMtFd2wGnZECT0kuIB9k=";
            "https://github.com/input-output-hk/cardano-base"."41545ba3ac6b3095966316a99883d678b5ab8da8" =
              "sha256-OXKsJ1UTj5kJ9xaThM54ZmxFAiFINTPKd4JQa4dPmEU=";
            "https://github.com/input-output-hk/cardano-prelude"."bb4ed71ba8e587f672d06edf9d2e376f4b055555" =
              "sha256-kgX3DKyfjBb8/XcDEd+/adlETsFlp5sCSurHWgsFAQI=";
            "https://github.com/input-output-hk/cardano-addresses"."71006f9eb956b0004022e80aadd4ad50d837b621" =
              "sha256-Eyu7PVYk1oQLp/Hd43S2PW+PojyAT/Rr48Xng6sbtIU=";
            "https://github.com/input-output-hk/cardano-wallet"."a5085acbd2670c24251cf8d76a4e83c77a2679ba" =
              "sha256-A3im2IkoumUx3NzgPooaXGC18/iYxbEooMa9ho93/6o=";
            "https://github.com/input-output-hk/ouroboros-network"."d2d219a86cda42787325bb8c20539a75c2667132" =
              "sha256-fZ6FfG2z6HWDxjIHycLPSQHoYtfUmWZOX7lfAUE+s6M=";
            "https://github.com/input-output-hk/iohk-monitoring-framework"."46f994e216a1f8b36fe4669b47b2a7011b0e153c" =
              "QE3QRpIHIABm+qCP/wP4epbUx0JmSJ9BMePqWEd3iMY=";
            "https://github.com/input-output-hk/cardano-ledger"."1a9ec4ae9e0b09d54e49b2a40c4ead37edadcce5" =
              "sha256-lRNfkGMHnpPO0T19FZY5BnuRkr0zTRZIkxZVgHH0fys=";
            "https://github.com/input-output-hk/cardano-node"."814df2c146f5d56f8c35a681fe75e85b905aed5d" =
              "sha256-M+YnF7Zj/7QK2pu0T75xNVaX0eEeijtBH8yz+jEHIMM=";
            "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a" =
              "uQx+SEYsCH7JcG3xAT0eJck9yq3y0cvx49bvItLLer8=";
            "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d" =
              "Hesb5GXSx0IwKSIi42ofisVELcQNX6lwHcoZcbaDiqc=";
            "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba" =
              "z9ut0y6umDIjJIRjz9KSvKgotuw06/S8QDwOtVdGiJ0=";
            "https://github.com/nomeata/tasty-expected-failure.git"."33b71e694b954e35c05859fff3ca886d8cfe5bfe" =
              "C/IWktTILklfPEAht/RE4IC8to6MrvrLmmbqgWsJlIM=";
          };
        };
    in {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      # this could be done automatically, but would reduce readability
      packages = perSystem (system: self.flake.${system}.packages);
      checks = perSystem (system: self.flake.${system}.checks);
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test" {
          nativeBuildInputs = builtins.attrValues self.checks.${system};
        } "touch $out");
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system: self.flake.${system}.devShell);
    };
}
