# This derivation demonstrates customizing Emacs with Nix.

{
  # Reference to `nixpkgs`.
  pkgs ? (import ./pkgs.nix).pkgs,

  # Extra user-defined Elisp packages not found in `nixpkgs`.
  userEmacsPackages ? (import ./user-packages.nix),

  # Selector of standard Elisp packages defined in `nixpkgs`.
  standardEmacsPackages ? (import ./packages.nix),

  # Elisp file to auto-load on startup. Customize a `default.el` file
  # as you would `~/.emacs`.
  defaultElisp ? ./default.el
}:

let

  userPackages = userEmacsPackages { pkgs = pkgs; };

  defaultPackage = pkgs.emacsPackages.trivialBuild {
    pname = "default";
    src = defaultElisp;
    packageRequires = (standardEmacsPackages { epkgs = pkgs.emacsPackages; }) ++ userPackages;
  };

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
