{
  pkgs ? import <nixpkgs>{},
  t0yv0-ware ? import ./t0yv0-ware.nix { pkgs = pkgs; }
}:

let

  matching = x: ty:
    pkgs.lib.hasSuffix "default.el" x ||
    builtins.match ".*snippets.*" x != null;

  sources = pkgs.lib.cleanSourceWith {
    filter = matching;
    src = ./.;
  };

in pkgs.emacsPackages.trivialBuild {
  pname = "defaultel";
  src = sources;
  postBuild = ''
   find $src
   mkdir -p $out/share/emacs/site-lisp
   cp -r $src/snippets $out/share/emacs/site-lisp/
  '';
  packageRequires = [
    pkgs.emacsPackages.use-package
    t0yv0-ware
  ];
}
