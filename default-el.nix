{
  pkgs ? import <nixpkgs>{},
  epkgs ? (pkgs.emacsPackagesFor pkgs.emacs29),
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

  treesitter = import ./ts.nix {
    pkgs = pkgs;
  };

in epkgs.trivialBuild {
  pname = "defaultel";
  src = sources;
  postBuild = ''
   mkdir -p $out/share/emacs/site-lisp
   cp -r $src/snippets $out/share/emacs/site-lisp/
   mkdir -p $out/share/emacs/site-lisp/tree-sitter
   ln -s ${treesitter}/go.so $out/share/emacs/site-lisp/tree-sitter/libtree-sitter-go.so
   ln -s ${treesitter}/gomod.so $out/share/emacs/site-lisp/tree-sitter/libtree-sitter-gomod.so
  '';
  packageRequires = [
    epkgs.avy
    epkgs.consult
    epkgs.corfu
    epkgs.dap-mode
    epkgs.edit-indirect
    epkgs.embark
    epkgs.embark-consult
    epkgs.expand-region
    epkgs.go-mode
    epkgs.haskell-mode
    epkgs.hydra
    epkgs.json-mode
    epkgs.magit
    epkgs.marginalia
    epkgs.mermaid-mode
    epkgs.nix-mode
    epkgs.orderless
    epkgs.ormolu
    epkgs.paredit
    epkgs.tide
    epkgs.tree-sitter-langs
    epkgs.typescript-mode
    epkgs.use-package
    epkgs.vertico
    epkgs.yaml-mode
    epkgs.yasnippet

    copilot
    t0yv0-ware
  ];
}
