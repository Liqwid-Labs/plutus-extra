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
            "https://github.com/input-output-hk/plutus-apps.git"."5ffdb6362b9ba3e7095beccde56df0280abf12d0"
              = "00vb22mrj3lzznfvz76qqjdk8r8jcvx5314s4bacddc1ppfw8dhv";
            "https://github.com/Quid2/flat"."ee59880f47ab835dbd73bea0847dab7869fc20d8"
              = "lRFND+ZnZvAph6ZYkr9wl9VAx41pb3uSFP8Wc7idP9M=";
            "https://github.com/input-output-hk/purescript-bridge"."366fc70b341e2633f3ad0158a577d52e1cd2b138"
              = "18j0rysfccbmfpbw2d1rsjkpd5h84alpsn6b5rwzdxw9h5vqi9m5";
            "https://github.com/input-output-hk/servant-purescript"."ebea59c7bdfc0338d83fca772b9a57e28560bcde"
              = "0gjcq4y61kwb4w70pnswn5dp23wd13dac8d9hz84j374cm1kshsn";
            "https://github.com/input-output-hk/cardano-crypto"."f73079303f663e028288f9f4a9e08bcca39a923e"
              = "1n87i15x54s0cjkh3nsxs4r1x016cdw1fypwmr68936n3xxsjn6q";
            "https://github.com/input-output-hk/cardano-base"."41545ba3ac6b3095966316a99883d678b5ab8da8"
              = "0icq9y3nnl42fz536da84414av36g37894qnyw4rk3qkalksqwir";
            "https://github.com/input-output-hk/cardano-prelude"."bb4ed71ba8e587f672d06edf9d2e376f4b055555"
              = "00h10l5mmiza9819p9v5q5749nb9pzgi20vpzpy1d34zmh6gf1cj";
            "https://github.com/input-output-hk/cardano-addresses"."71006f9eb956b0004022e80aadd4ad50d837b621"
              = "11dl3fmq7ry5wdmz8kw07ji8yvrxnrsf7pgilw5q9mi4aqyvnaqk";
            "https://github.com/input-output-hk/cardano-wallet"."a5085acbd2670c24251cf8d76a4e83c77a2679ba"
              = "1apzfy7qdgf6l0lb3icqz3rvaq2w3a53xq6wvhqnbfi8i7cacy03";
            "https://github.com/input-output-hk/ouroboros-network"."d2d219a86cda42787325bb8c20539a75c2667132"
              = "18xk7r0h2pxrbx76d6flsxifh0a9rz1cj1rjqs1pbs5kdmy8b7kx";
            "https://github.com/input-output-hk/iohk-monitoring-framework"."46f994e216a1f8b36fe4669b47b2a7011b0e153c"
              = "QE3QRpIHIABm+qCP/wP4epbUx0JmSJ9BMePqWEd3iMY=";
            "https://github.com/input-output-hk/cardano-ledger"."1a9ec4ae9e0b09d54e49b2a40c4ead37edadcce5"
              = "0avzyiqq0m8njd41ck9kpn992yq676b1az9xs77977h7cf85y4wm";
            "https://github.com/input-output-hk/cardano-node"."814df2c146f5d56f8c35a681fe75e85b905aed5d"
              = "1hr00wqzmcyc3x0kp2hyw78rfmimf6z4zd4vv85b9zv3nqbjgrik";
            "https://github.com/input-output-hk/cardano-config"."e9de7a2cf70796f6ff26eac9f9540184ded0e4e6"
              = "1wm1c99r5zvz22pdl8nhkp13falvqmj8dgkm8fxskwa9ydqz01ld";
            "https://github.com/input-output-hk/optparse-applicative"."7497a29cb998721a9068d5725d49461f2bba0e7a"
              = "1gvsrg925vynwgqwplgjmp53vj953qyh3wbdf34pw21c8r47w35r";
            "https://github.com/input-output-hk/Win32-network"."3825d3abf75f83f406c1f7161883c438dac7277d"
              = "Hesb5GXSx0IwKSIi42ofisVELcQNX6lwHcoZcbaDiqc=";
            "https://github.com/input-output-hk/goblins"."cde90a2b27f79187ca8310b6549331e59595e7ba"
              = "z9ut0y6umDIjJIRjz9KSvKgotuw06/S8QDwOtVdGiJ0=";
            "https://github.com/input-output-hk/plutus.git"."cc72a56eafb02333c96f662581b57504f8f8992f"
              = "1w89ikv3jsg5x9xf7qpcjnix3nf016z0xpf48q5238h4ngvcqp9y";
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
