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
            "https://github.com/input-output-hk/plutus.git"."65bad0fd53e432974c3c203b1b1999161b6c2dce" =
              "sha256-0l8kWR9R0XkkJInbKP/1l8e5jCVhZQ7fVo7IRaXepQ8=";
            "https://github.com/input-output-hk/plutus-apps.git"."34fe6eeff441166fee0cd0ceba68c1439f0e93d2" =
              "sha256-UULYQppoNjj+EOcV75UT3DOwJF+d609FOYsZZFeAQcM=";
            "https://github.com/Quid2/flat.git"."ee59880f47ab835dbd73bea0847dab7869fc20d8" =
              "lRFND+ZnZvAph6ZYkr9wl9VAx41pb3uSFP8Wc7idP9M=";
            "https://github.com/input-output-hk/purescript-bridge.git"."366fc70b341e2633f3ad0158a577d52e1cd2b138" =
              "paaId4GJ9/Z5LstYfakiCJZ2p9Q5NMHXdXUx5rTPQKI=";
            "https://github.com/input-output-hk/servant-purescript.git"."ebea59c7bdfc0338d83fca772b9a57e28560bcde" =
              "VkM9Q2XkDEnQh6khptoIjQ9xW7Fc2wsOJ4vPYDzBTD4=";
            "https://github.com/input-output-hk/cardano-crypto.git"."f73079303f663e028288f9f4a9e08bcca39a923e" =
              "sha256-2Fipex/WjIRMrvx6F3hjJoAeMtFd2wGnZECT0kuIB9k=";
            "https://github.com/input-output-hk/cardano-base"."654f5b7c76f7cc57900b4ddc664a82fc3b925fb0" =
              "sha256-JKpOlruMX5sr9eaQ3AuOppCbBjQIRKwF4ny20tdPnUg=";
            "https://github.com/input-output-hk/cardano-prelude"."bb4ed71ba8e587f672d06edf9d2e376f4b055555" =
              "sha256-kgX3DKyfjBb8/XcDEd+/adlETsFlp5sCSurHWgsFAQI=";
            "https://github.com/input-output-hk/cardano-addresses"."d2f86caa085402a953920c6714a0de6a50b655ec" =
              "XgXQKJHRKAFwIjONh19D/gKE0ARlhMXXcV74eZpd0lw=";
            "https://github.com/input-output-hk/cardano-wallet"."760140e238a5fbca61d1b286d7a80ece058dc729" =
              "sha256-JuYH5pAF7gOsliES0Beo86PinoBmmKXWShXT3NqVlgQ=";
            "https://github.com/input-output-hk/ouroboros-network"."d613de3d872ec8b4a5da0c98afb443f322dc4dab" =
              "sha256-FNYcUjoy0ZpletEXUIAMbag2Hwb9K3bDRl793NyNy1E=";
            "https://github.com/input-output-hk/iohk-monitoring-framework"."46f994e216a1f8b36fe4669b47b2a7011b0e153c" =
              "QE3QRpIHIABm+qCP/wP4epbUx0JmSJ9BMePqWEd3iMY=";
            "https://github.com/input-output-hk/cardano-ledger"."bf008ce028751cae9fb0b53c3bef20f07c06e333" =
              "HTPOmVOXgBD/3uAxZip/HSttaKcJ+uImYDbuwANAw1c=";
            "https://github.com/input-output-hk/cardano-node.git"."4f65fb9a27aa7e3a1873ab4211e412af780a3648" =
              "sha256-PWcWv2RKsxHrsDs+ZjNeCOJlfmIW9CGilPA+UDN2aQI=";
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
