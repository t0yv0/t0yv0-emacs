{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-23.11;
    nixpkgs_22_11.url = github:NixOS/nixpkgs/22.11;
    copilot_flake.url = github:t0yv0/copilot.el?rev=a0a8a69cf924c2b45f1ad3d0eb9fbe3a762e58f4;
  };

  outputs = { self, nixpkgs, nixpkgs_22_11, copilot_flake }: let

    version = self.rev or "dirty";

    packages = sys: emacs-flavor: let
      pkgs = import nixpkgs { system = sys; };
      pkgs_22_11 = import nixpkgs_22_11 { system = sys; };
      epkgs = pkgs.emacsPackagesFor (emacs-flavor pkgs);
      treesitter = pkgs.tree-sitter.withPlugins (_: pkgs.tree-sitter.allGrammars);
      mermaid = pkgs_22_11.nodePackages.mermaid-cli;
      copilot = (builtins.getAttr sys copilot_flake.packages).default;

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

      t0yv0-treesit = epkgs.trivialBuild {
        pname = "t0yv0-treesit";
        version = "${version}";
        src = [ ./t0yv0-treesit.el ];
      };

      prebuilt = pkgs.symlinkJoin {
        name = "t0yv0-emacs-prebuilt-${version}";
        version = "${version}";
        paths = [
          epkgs.avy
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
          epkgs.mermaid-mode
          epkgs.multiple-cursors
          epkgs.nix-mode
          epkgs.ormolu
          epkgs.pretty-hydra
          epkgs.reformatter
          epkgs.transient
          epkgs.typescript-mode
          epkgs.with-editor
          epkgs.yaml-mode
          t0yv0-basics
          t0yv0-treesit
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
        copilot
        default-el
        epkgs.corfu
        epkgs.diminish
        epkgs.doom-modeline
        epkgs.envrc
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
      default-el = default-el;
      mermaid = mermaid;
      prebuilt = prebuilt;
      t0yv0-basics = t0yv0-basics;
      t0yv0-treesit = t0yv0-treesit;
      treesitter = treesitter;

      # Needs a manual step to install on Mac OS. `nix build && cd result`, select and open all the
      # font files simultaneously, this prompts the installer.
      iosevka = pkgs.iosevka;

      # On Macs need to copy these manually into ~/Library/Spelling for jinx to work.
      dicts = pkgs.hunspellDicts.en_US;
    };

  in {
    packages = {
      "x86_64-darwin"  = packages "x86_64-darwin"  (pkgs: pkgs.emacs29-macport);
      "aarch64-darwin" = packages "aarch64-darwin" (pkgs: pkgs.emacs29-macport);
      "x86_64-linux"   = packages "x86_64-linux"   (pkgs: pkgs.emacs29);
    };
  };
}
