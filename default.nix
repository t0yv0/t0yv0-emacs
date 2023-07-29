# This derivation demonstrates customizing Emacs with Nix.

{
  # Reference to `nixpkgs`.
  pkgs ? (import ./pkgs.nix).pkgs,

  # Extra user-defined Elisp packages not found in `nixpkgs`.
  userEmacsPackages ? (import ./user-packages.nix),

  # Selector of standard Elisp packages defined in `nixpkgs`.
  standardEmacsPackages ? (import ./packages.nix),
}:

let

  userPackages = userEmacsPackages { pkgs = pkgs; };

  defaultPackage = import ./default-el.nix { pkgs = pkgs; };

  emacsPackages = epkgs: (standardEmacsPackages { epkgs = epkgs; }) ++ userPackages ++ [defaultPackage];

  emacs = (pkgs.emacsPackagesFor pkgs.emacsNativeComp).emacsWithPackages(emacsPackages);

in

pkgs.stdenv.mkDerivation {
  name = "t0yv0-emacs-0.0.1";
  builder = "${pkgs.bash}/bin/bash";
  coreutils = pkgs.coreutils;
  emacs = emacs;
  args = [ ./builder.sh ];
}
