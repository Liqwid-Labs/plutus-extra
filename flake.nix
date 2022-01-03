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
              pkgs.hlint
              pkgs.haskellPackages.fourmolu
              pkgs.entr
              pkgs.nixfmt
              pkgs.haskellPackages.cabal-fmt
            ];

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
            "https://github.com/input-output-hk/plutus.git"."3f089ccf0ca746b399c99afe51e063b0640af547" =
              "BhGQPiCv4UxVs0XEdMMddaNWiztmkoeJotpW/lrtqNs=";
            "https://github.com/input-output-hk/plutus-apps.git"."404af7ac3e27ebcb218c05f79d9a70ca966407c9" =
              "+T9TGzHEzyfixBysxLwy5VWVrL5xqKF5pcbRlHQr+wI=";
            "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" =
              "lRFND+ZnZvAph6ZYkr9wl9VAx41pb3uSFP8Wc7idP9M=";
            "https://github.com/input-output-hk/purescript-bridge.git"."366fc70b341e2633f3ad0158a577d52e1cd2b138" =
              "paaId4GJ9/Z5LstYfakiCJZ2p9Q5NMHXdXUx5rTPQKI=";
            "https://github.com/input-output-hk/servant-purescript.git"."ebea59c7bdfc0338d83fca772b9a57e28560bcde" =
              "VkM9Q2XkDEnQh6khptoIjQ9xW7Fc2wsOJ4vPYDzBTD4=";
            "https://github.com/input-output-hk/cardano-crypto.git"."07397f0e50da97eaa0575d93bee7ac4b2b2576ec" =
              "oxIOVlgm07FAEmgGRF1C2me9TXqVxQulEOcJ22zpTRs=";
            "https://github.com/input-output-hk/cardano-base"."4ea7e2d927c9a7f78ddc69738409a5827ab66b98" =
              "zbjq43Bnhv1/LhJCFlI8gdd61dGvVlkEa6wkCvLqEFg=";
            "https://github.com/input-output-hk/cardano-prelude"."fd773f7a58412131512b9f694ab95653ac430852" =
              "BtbT5UxOAADvQD4qTPNrGfnjQNgbYNO4EAJwH2ZsTQo=";
            "https://github.com/input-output-hk/cardano-addresses"."d2f86caa085402a953920c6714a0de6a50b655ec" =
              "XgXQKJHRKAFwIjONh19D/gKE0ARlhMXXcV74eZpd0lw=";
            "https://github.com/j-mueller/cardano-wallet"."6be73ab852c0592713dfe78218856d4a8a0ee69e" =
              "5IZuqlE/4aGH3TEuGYQsZwOpI/Q7DYzJ4q3stuqGpWc=";
            "https://github.com/input-output-hk/ouroboros-network"."1f4973f36f689d6da75b5d351fb124d66ef1057d" =
              "lwTgyoZBQAaU6Sh7BouGJGUvK1tSVrWhJP63v7MpwKA=";
            "https://github.com/input-output-hk/iohk-monitoring-framework"."46f994e216a1f8b36fe4669b47b2a7011b0e153c" =
              "QE3QRpIHIABm+qCP/wP4epbUx0JmSJ9BMePqWEd3iMY=";
            "https://github.com/input-output-hk/cardano-ledger-specs"."bf008ce028751cae9fb0b53c3bef20f07c06e333" =
              "HTPOmVOXgBD/3uAxZip/HSttaKcJ+uImYDbuwANAw1c=";
            "https://github.com/input-output-hk/cardano-node.git"."b6ca519f97a0e795611a63174687e6bb70c9f752" =
              "tuEtSCJOk1MA9sguxL13XLa+qHaz//v7eNyhxHC9tHw=";
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
