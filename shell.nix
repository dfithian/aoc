{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.stack
    pkgs.cargo
    pkgs.rustfmt
    pkgs.rust-analyzer
  ];
}
