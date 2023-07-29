# This derivation demonstrates customizing Emacs with Nix.

{
  pkgs ? import <nixpkgs>{},
}:

let
  myEmacs = pkgs.emacsNativeComp;
  emacsWithPackages = (pkgs.emacsPackagesFor myEmacs).emacsWithPackages;

  myEmacsPackages = epkgs: [
    (import ./default-el.nix { pkgs = pkgs; epkgs = epkgs; })
  ];

in

pkgs.stdenv.mkDerivation {
  name = "t0yv0-emacs-0.0.1";
  builder = "${pkgs.bash}/bin/bash";
  coreutils = pkgs.coreutils;
  emacs = emacsWithPackages myEmacsPackages;
  args = [ ./builder.sh ];
}
