{
  pkgs ? import <nixpkgs>{},
  epkgs ? (pkgs.emacsPackagesFor pkgs.emacs29),
  t0yv0-ware ? import ./t0yv0-ware.nix { pkgs = pkgs; epkgs = epkgs; },
  copilot ? import ./copilot.nix { pkgs = pkgs; epkgs = epkgs; },
  pkgs_22_11
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

  mermaid = pkgs_22_11.nodePackages.mermaid-cli;

in epkgs.trivialBuild {
  pname = "defaultel";
  src = sources;
  postBuild = ''
   mkdir -p $out/share/emacs/site-lisp
   mkdir -p $out/share/emacs/site-lisp/bin
   cp -r $src/snippets $out/share/emacs/site-lisp/
   mkdir -p $out/share/emacs/site-lisp/tree-sitter
   ln -s ${treesitter}/go.so $out/share/emacs/site-lisp/tree-sitter/libtree-sitter-go.so
   ln -s ${treesitter}/gomod.so $out/share/emacs/site-lisp/tree-sitter/libtree-sitter-gomod.so
   ln -s ${mermaid}/bin/mmdc $out/share/emacs/site-lisp/bin/mmdc
  '';
  packageRequires = [
    epkgs.avy
    epkgs.consult
    epkgs.corfu
    epkgs.dap-mode
    epkgs.diminish
    epkgs.edit-indirect
    epkgs.embark
    epkgs.embark-consult
    epkgs.expand-region
    epkgs.git-link
    epkgs.go-mode
    epkgs.haskell-mode
    epkgs.hydra
    # Note on JINX: this spell-checking package requires additional packages; on NixOS installing
    # pkgs.nuspell and pkgs.hunspellDicts.en_US works fine; TBD on MacOS. I have not taken the time
    # to find how to inline this into t0yv0-emacs so it installs a local copy.
    epkgs.jinx
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
