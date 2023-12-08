{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-23.11;
    nixpkgs_22_11.url = github:NixOS/nixpkgs/22.11;
    copilot_src.url = github:zerolfx/copilot.el?rev=4f83577b3a3c120e9b1063b5a793c20a6ed9ece0;
    copilot_src.flake = false;
  };

  outputs = { self, nixpkgs, nixpkgs_22_11, copilot_src }: let

    packages = sys: let
      pkgs = import nixpkgs { system = sys; };
      pkgs_22_11 = import nixpkgs_22_11 { system = sys; };
      version = self.rev or "dirty";

      epkgs = pkgs.emacsPackagesFor pkgs.emacs29;

      t0yv0-ware = epkgs.trivialBuild {
        pname = "t0yv0-ware";
        src = ./t0yv0-ware.el;
        version = "${version}";
        packageRequires = [
          epkgs.dash
          epkgs.consult
          epkgs.go-mode
          epkgs.markdown-mode
          epkgs.mermaid-mode
          epkgs.vterm
        ];
      };

      copilot = epkgs.trivialBuild {
        pname = "copilot";
        version = "${version}";
        src = copilot_src;
        packageRequires = [
          epkgs.dash
          epkgs.s
          epkgs.editorconfig
        ];
        postBuild = ''
          mkdir -p $out/share/emacs/site-lisp
          cp -r $src/dist $out/share/emacs/site-lisp/
        '';
      };

      matching = x: ty:
        pkgs.lib.hasSuffix "default.el" x ||
        builtins.match ".*snippets.*" x != null;

      sources = pkgs.lib.cleanSourceWith {
        filter = matching;
        src = ./.;
      };

      treesitter = pkgs.tree-sitter.withPlugins (_: pkgs.tree-sitter.allGrammars);

      mermaid = pkgs_22_11.nodePackages.mermaid-cli;

      t0yv0-emacs-lisp = epkgs.trivialBuild {
        pname = "t0yv0-emacs-lisp";
        version = "${version}";
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
          epkgs.multiple-cursors
          epkgs.nix-mode
          epkgs.orderless
          epkgs.ormolu
          epkgs.paredit
          epkgs.selected
          epkgs.tide
          epkgs.tree-sitter-langs
          epkgs.typescript-mode
          epkgs.use-package
          epkgs.vertico
          epkgs.wgrep
          epkgs.yaml-mode
          epkgs.yasnippet

          copilot
          t0yv0-ware
        ];
      };

      t0yv0-emacs = pkgs.stdenv.mkDerivation {
        name = "t0yv0-emacs-${version}";
        version = "${version}";
        builder = "${pkgs.bash}/bin/bash";
        coreutils = pkgs.coreutils;
        emacs = epkgs.emacsWithPackages (epkgs: [
          t0yv0-emacs-lisp
        ]);
        args = [ ./builder.sh "${version}" ];
      };

    in {
      copilot = copilot;
      default = t0yv0-emacs;
      mermaid = mermaid;
      t0yv0-emacs = t0yv0-emacs;
      t0yv0-emacs-lisp = t0yv0-emacs-lisp;
      t0yv0-ware = t0yv0-ware;
      treesitter = treesitter;
    };

  in {
    packages = builtins.listToAttrs (builtins.map (sys: {
      name = sys;
      value = packages sys;
    }) [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ]);
  };
}
