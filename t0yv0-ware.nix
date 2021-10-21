{ pkgs ? import <nixpkgs>{} }:

pkgs.emacsPackages.trivialBuild {
  pname = "t0yv0-ware";
  src = ./t0yv0-ware.el;
}
