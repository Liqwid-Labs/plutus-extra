with import ./nix { };
(plutus.plutus.haskell.project.shellFor (pab.env_variables // {

  # Select packages who's dependencies should be added to the shell env
  packages = ps: [ ];
  
  # Select packages which should be added to the shell env, with their dependencies
  # Should try and get the extra cardano dependencies in here...
  additional = ps:
    with ps; [
      plutus-pab
      plutus-tx
      plutus-tx-plugin
      plutus-contract
      plutus-ledger-api
      pab.plutus_ledger_with_docs
      plutus-core
      playground-common
      prettyprinter-configurable
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

      # Using plutus-latest, we get access to hls with ghc 8.10.4.20210212
      plutus-latest.plutus.haskell-language-server

      # hls doesn't support preprocessors yet so this has to exist in PATH
      haskellPackages.record-dot-preprocessor


      # Graphviz Diagrams for documentation
      graphviz

      ### Pab
      pab.plutus_pab_client

      ### Example contracts
      plutus.plutus-atomic-swap
      plutus.plutus-currency

    ] ++ (builtins.attrValues pab.plutus_pab_exes);

  buildInputs = (with plutus.pkgs;
    [ zlib pkg-config libsodium systemd ]
    ++ (lib.optionals (!stdenv.isDarwin) [ R ]));

}))
