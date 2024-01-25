{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  packages = with pkgs; [
    (haskellPackages.ghcWithPackages (p: with p; [ vector ]))
  ];
}