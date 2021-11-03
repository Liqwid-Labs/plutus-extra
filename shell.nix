with import ./nix { };
(plutus-apps.plutus-apps.haskell.project.shellFor {

  # Select packages who's dependencies should be added to the shell env
  packages = ps: [ ];

  # Select packages which should be added to the shell env, with their dependencies
  # Should try and get the extra cardano dependencies in here...
  additional = ps:
    with ps; [
      plutus-tx
      plutus-tx-plugin
      plutus-ledger-api
      plutus-core
      prettyprinter-configurable
      cardano-crypto-praos
      cardano-crypto-class
      plutus-contract
      playground-common
      plutus-use-cases
    ];

  withHoogle = true;

  # Extra haskell tools (arg passed on to mkDerivation)
  # Using the plutus.pkgs to use nixpkgs version from plutus (nixpkgs-unstable, mostly)
  propagatedBuildInputs = with pkgs;
    [
      # Haskell Tools
      stack
      cabal-install
      haskellPackages.fourmolu
      entr
      git
      ghc
      nixfmt
      plutus.plutus.hlint

      plutus.plutus.haskell-language-server

      # hls doesn't support preprocessors yet so this has to exist in PATH
      haskellPackages.record-dot-preprocessor

      # Graphviz Diagrams for documentation
      graphviz

      ### Example contracts
      plutus-apps.plutus-pab-examples

    ];

  buildInputs = (with plutus-apps.pkgs;
    [ zlib pkg-config libsodium-vrf R lzma ]
    ++ (lib.optionals (!stdenv.isDarwin) [ systemd ]));

})
