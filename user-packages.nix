{ pkgs ? import <nixpkgs>{} }:

let
  t0yv0-ware = (import ./t0yv0-ware.nix) { pkgs = pkgs; };

in
[
  t0yv0-ware
]
