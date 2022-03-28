# This derivation demonstrates customizing Emacs with Nix.

{
  # Reference to `nixpkgs`.
  pkgs ? (import ./pkgs.nix).pkgs,

  # Extra user-defined Elisp packages not found in `nixpkgs`.
  userEmacsPackages ? (import ./user-packages.nix),

  # Selector of standard Elisp packages defined in `nixpkgs`.
  standardEmacsPackages ? (import ./packages.nix),

  # Config flags such as the experimental `--with-native-compilation`
  # flag to speed up execution.
  extraConfigureFlags ? [ "--with-native-compilation" ],

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

  patchedEmacs = pkgs.emacs.overrideAttrs(old: {
    configureFlags = old.configureFlags ++ extraConfigureFlags;
    patches = old.patches ++ [ ./shell-compile-long-line.patch ];
  });

  emacs = (pkgs.emacsPackagesFor patchedEmacs).emacsWithPackages(epkgs:
    (standardEmacsPackages { epkgs = epkgs; }) ++ userPackages ++ [defaultPackage]
  );

in

pkgs.stdenv.mkDerivation {
  name = "t0yv0-emacs-0.0.1";
  builder = "${pkgs.bash}/bin/bash";
  coreutils = pkgs.coreutils;
  emacs = emacs;
  args = [ ./builder.sh ];
}
