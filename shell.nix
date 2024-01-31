{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  packages = with pkgs; [
    (haskellPackages.ghcWithPackages (p: with p; [ 
      vector 
      x86-64bit 
      temporary 
      cmdargs 
      file-embed 
    ]))
    fasm
    hlint
  ];
}