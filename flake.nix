{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-23.11;
    nixpkgs_22_11.url = github:NixOS/nixpkgs/22.11;
    nixpkgs_darwin.url = github:NixOS/nixpkgs/nixpkgs-23.11-darwin;
    copilot_flake.url = github:t0yv0/copilot.el/v20240323;
    treesitedit_flake.url = github:t0yv0/treesitedit.el/main;
    dape_src.url = github:svaante/dape?rev=d1a96de51cbee7c410d1f2680f860d09048e2fc5;
    dape_src.flake = false;
  };

  outputs = { self, nixpkgs, nixpkgs_22_11, nixpkgs_darwin, copilot_flake, dape_src, treesitedit_flake }: let

    version = self.rev or "dirty";

    packages = nixpkgs: sys: emacs-flavor: let
      pkgs = import nixpkgs { system = sys; };
      pkgs_22_11 = import nixpkgs_22_11 { system = sys; };
      epkgs = pkgs.emacsPackagesFor (emacs-flavor pkgs);
      treesitter = pkgs.tree-sitter.withPlugins (_: pkgs.tree-sitter.allGrammars);
      mermaid = pkgs_22_11.nodePackages.mermaid-cli;
      copilot = (builtins.getAttr sys copilot_flake.packages).default;
      treesitedit = (builtins.getAttr sys treesitedit_flake.packages).default;

      jsonrpc = epkgs.elpaBuild {
        pname = "jsonrpc";
        ename = "jsonrpc";
        version = "1.0.25";
        src = pkgs.fetchurl {
          url = "https://elpa.gnu.org/packages/jsonrpc-1.0.25.tar";
          sha256 = "sha256-zsBYbvpNWncnLpkUImgCzcJWhDaaiHloHEWNSjs4jEI=";
        };
        packageRequires = [];
        meta = {
          homepage = "https://elpa.gnu.org/packages/jsonrpc.html";
        };
      };

      eglot = epkgs.elpaBuild {
        pname = "eglot";
        ename = "eglot";
        version = "1.17";
        src = pkgs.fetchurl {
          url = "https://elpa.gnu.org/packages/eglot-1.17.tar";
          sha256 = "sha256-flJX2hEj34Ah9eeyhWz0RSYdvmwKPU5kA7bh0xBKVrE=";
        };
        packageRequires = [jsonrpc];
        meta = {
          homepage = "https://elpa.gnu.org/packages/eglot.html";
        };
      };

      dape = epkgs.trivialBuild {
        pname = "dape";
        version = "0.0.1";
        src = dape_src;
        packageRequires = [jsonrpc];
      };

      t0yv0-basics = epkgs.trivialBuild {
        pname = "t0yv0-basics";
        version = "${version}";
        src = [ ./t0yv0-basics.el ];
        packageRequires = [
          epkgs.consult
          epkgs.dash
          epkgs.vterm
        ];
      };

      prebuilt = pkgs.symlinkJoin {
        name = "t0yv0-emacs-prebuilt-${version}";
        version = "${version}";
        paths = [
          epkgs.ace-window
          epkgs.avy
          epkgs.emacsql
          epkgs.embark
          epkgs.embark-consult
          epkgs.expand-region
          epkgs.git-commit
          epkgs.git-link
          epkgs.go-mode
          epkgs.haskell-mode
          epkgs.magit
          epkgs.magit-section
          epkgs.major-mode-hydra
          epkgs.markdown-mode
          epkgs.mermaid-mode
          epkgs.multiple-cursors
          epkgs.nix-mode
          epkgs.org-roam
          epkgs.ormolu
          epkgs.pretty-hydra
          epkgs.reformatter
          epkgs.transient
          epkgs.typescript-mode
          epkgs.with-editor
          epkgs.yaml-mode
          dape
          t0yv0-basics
        ];
      };

      bootstrap = pkgs.stdenv.mkDerivation {
        name = "t0yv0-emacs-bootstrap-${version}";
        version = "${version}";
        builder = "${pkgs.bash}/bin/bash";
        coreutils = pkgs.coreutils;
        args = [ ./bootstrap.sh "${prebuilt}" ./default.el
                 "${treesitter}" "${mermaid}/bin/mmdc" ./snippets ];
      };

      default-el = epkgs.trivialBuild {
        pname = "t0yv0-emacs-default-el";
        version = "${version}";
        src = [ "${bootstrap}" ];
        packageRequires = [
          t0yv0-basics
          epkgs.hydra
        ];
      };

      eager-packages = epkgs: [
        treesitedit
        copilot
        eglot
        jsonrpc
        default-el
        epkgs.corfu
        epkgs.diminish
        epkgs.doom-modeline
        epkgs.envrc
        epkgs.forge
        epkgs.gptel
        epkgs.hydra
        # Note on JINX: this spell-checking package requires additional packages; on NixOS
        # installing pkgs.nuspell and pkgs.hunspellDicts.en_US works fine; TBD on MacOS. I have not
        # taken the time to find how to inline this into t0yv0-emacs so it installs a local copy.
        epkgs.jinx
        epkgs.marginalia
        epkgs.orderless
        epkgs.vertico
        epkgs.wgrep
        epkgs.yasnippet
      ];

      default = epkgs.emacsWithPackages eager-packages;

    in {
      default = default;
      copilot = copilot;
      bootstrap = bootstrap;
      eglot = eglot;
      dape = dape;
      default-el = default-el;
      jsonrpc = jsonrpc;
      mermaid = mermaid;
      prebuilt = prebuilt;
      t0yv0-basics = t0yv0-basics;
      treesitter = treesitter;

      # Needs a manual step to install on Mac OS. `nix build && cd result`, select and open all the
      # font files simultaneously, this prompts the installer.
      iosevka = pkgs.iosevka;

      # On Macs need to copy these manually into ~/Library/Spelling for jinx to work.
      dicts = pkgs.hunspellDicts.en_US;
    };

  in {
    packages = {
      "x86_64-darwin" = packages nixpkgs_darwin "x86_64-darwin"  (pkgs: pkgs.emacs29-macport);
      "aarch64-darwin" = packages nixpkgs_darwin "aarch64-darwin" (pkgs: pkgs.emacs29-macport);
      "x86_64-linux" = packages nixpkgs "x86_64-linux" (pkgs: pkgs.emacs29);
    };
  };
}
