{ pkgs ? import <nixpkgs>{},
  epkgs ? pkgs.emacsPackages
}:

epkgs.trivialBuild {
  pname = "t0yv0-ware";
  src = ./t0yv0-ware.el;
  packageRequires = [
    epkgs.dash
    epkgs.consult
    epkgs.go-mode
    epkgs.markdown-mode
    epkgs.mermaid-mode
    epkgs.vterm
  ];
}
