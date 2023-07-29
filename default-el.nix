{
  pkgs ? import <nixpkgs>{},
  epkgs ? pkgs.emacsPackages,
  t0yv0-ware ? import ./t0yv0-ware.nix { pkgs = pkgs; epkgs = epkgs; },
  copilot ? import ./copilot.nix { pkgs = pkgs; epkgs = epkgs; }
}:

let

  matching = x: ty:
    pkgs.lib.hasSuffix "default.el" x ||
    builtins.match ".*snippets.*" x != null;

  sources = pkgs.lib.cleanSourceWith {
    filter = matching;
    src = ./.;
  };

in epkgs.trivialBuild {
  pname = "defaultel";
  src = sources;
  postBuild = ''
   find $src
   mkdir -p $out/share/emacs/site-lisp
   cp -r $src/snippets $out/share/emacs/site-lisp/
  '';
  packageRequires = [
    epkgs.company
    epkgs.consult
    epkgs.consult-flycheck
    epkgs.csharp-mode
    epkgs.dap-mode
    epkgs.edit-indirect
    epkgs.embark
    epkgs.embark-consult
    epkgs.go-mode
    epkgs.haskell-mode
    epkgs.hydra
    epkgs.json-mode
    epkgs.lsp-ui
    epkgs.magit
    epkgs.marginalia
    epkgs.mermaid-mode
    epkgs.nix-mode
    epkgs.orderless
    epkgs.ormolu
    epkgs.paredit
    epkgs.tide
    epkgs.typescript-mode
    epkgs.use-package
    epkgs.vertico
    epkgs.yaml-mode
    epkgs.yasnippet

    copilot
    t0yv0-ware
  ];
}
