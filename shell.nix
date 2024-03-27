{ pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-23.05.tar.gz") {} }:

with pkgs;
let
  haskell-stack = haskellPackages.ghcWithPackages (hpkgs: with hpkgs; [
    stack
  ]);
in
pkgs.stdenvNoCC.mkDerivation {
  name = "shell";
  buildInputs = [
    haskell-stack
  ];
}
