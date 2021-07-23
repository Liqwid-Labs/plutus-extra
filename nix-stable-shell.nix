let 
  nixpkgs-2009 = (import ./nix/sources.nix).nixpkgs-2009;

  pkgs = import nixpkgs-2009 {};

in

pkgs.mkShell {
  buildInputs = [pkgs.nix];
}

