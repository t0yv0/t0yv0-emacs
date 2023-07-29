{ pkgs ? import <nixpkgs>{},
  epkgs ? pkgs.emacsPackages
}:

epkgs.trivialBuild {
  pname = "t0yv0-ware";
  src = ./t0yv0-ware.el;
  packageRequires = [
    epkgs.dash
    epkgs.consult
    epkgs.markdown-mode
    epkgs.vterm
  ];
}
