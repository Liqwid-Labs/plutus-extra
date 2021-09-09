#!/usr/bin/env bash
nix-instantiate nix/update.nix --eval --strict --json | jq -r '.[] | "nix run nixpkgs.niv -c niv update \(.name) -r \(.tag)"' | bash -x
