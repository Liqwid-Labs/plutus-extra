{ sourcesFile ? ./sources.json, system ? builtins.currentSystem }: rec {
  sources = import ./sources.nix { inherit sourcesFile system; };
  plutus = import sources.plutus { };
  plutus-apps = import sources.plutus-apps { };
  pkgs = plutus.pkgs;
}
