# This derivation demonstrates customizing Emacs with Nix.

{
  pkgs ? import <nixpkgs>{},
  pkgs_22_11
}:

let
  myEmacs = pkgs.emacs29;

  emacsWithPackages = (pkgs.emacsPackagesFor myEmacs).emacsWithPackages;

  default-el = import ./default-el.nix { pkgs = pkgs; pkgs_22_11 = pkgs_22_11; };

  myEmacsPackages = epkgs: [
    default-el
  ];

in

pkgs.stdenv.mkDerivation {
  name = "t0yv0-emacs-0.0.1";
  builder = "${pkgs.bash}/bin/bash";
  coreutils = pkgs.coreutils;
  emacs = emacsWithPackages myEmacsPackages;
  args = [ ./builder.sh ];
}
