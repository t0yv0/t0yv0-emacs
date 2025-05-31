{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-25.05;
    flake-utils.url = "github:numtide/flake-utils";

    copilot_flake.url = github:t0yv0/copilot.el/nix;
    copilot_flake.inputs.nixpkgs.follows = "nixpkgs";
    copilot_flake.inputs.flake-utils.follows = "flake-utils";

    treesitedit_flake.url = github:t0yv0/treesitedit.el/main;
    treesitedit_flake.inputs.nixpkgs.follows = "nixpkgs";
    treesitedit_flake.inputs.flake-utils.follows = "flake-utils";

    testrun_flake.url = github:t0yv0/testrun.el/main;
    testrun_flake.inputs.nixpkgs.follows = "nixpkgs";
    testrun_flake.inputs.flake-utils.follows = "flake-utils";

    vterms_flake.url = github:t0yv0/vterms.el/main;
    vterms_flake.inputs.nixpkgs.follows = "nixpkgs";
    vterms_flake.inputs.flake-utils.follows = "flake-utils";

    ghauto_flake.url = github:t0yv0/gh-autolinks.el/main;
    ghauto_flake.inputs.nixpkgs.follows = "nixpkgs";
    ghauto_flake.inputs.flake-utils.follows = "flake-utils";

    dape_src.url = github:svaante/dape?rev=d1a96de51cbee7c410d1f2680f860d09048e2fc5;
    dape_src.flake = false;

    mcpel_src.url = github:lizqwerscott/mcp.el?rev=7f77145cc5dbfd0552b0cec2ad615c346b0574d8;
    mcpel_src.flake = false;

    gptel_src.url = github:karthink/gptel?rev=45814df5dca127cc2b0ec6d4e3daa1b7e57d8a5b;
    gptel_src.flake = false;
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    copilot_flake,
    dape_src,
    mcpel_src,
    gptel_src,
    treesitedit_flake,
    testrun_flake,
    vterms_flake,
    ghauto_flake,
  }: let

    version = self.rev or "dirty";

    # https://github.com/NixOS/nixpkgs/issues/395169
    emacs-overlay = final: prev: {
      emacs = prev.emacs.override { withNativeCompilation = false; };
    };

    overlay = final: prev: {
      t0yv0-emacs = final.callPackage ./package.nix {
        inherit version dape_src mcpel_src gptel_src;
        pkgs = final;
        epkgs = final.emacsPackagesFor final.emacs;
        stdenv = final.stdenv;
        mermaid-cli = final.nodePackages.mermaid-cli;
      };
    };

    out = system: let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          emacs-overlay
          treesitedit_flake.overlays.default
          testrun_flake.overlays.default
          vterms_flake.overlays.default
          ghauto_flake.overlays.default
          copilot_flake.overlays.default
        ];
      };
    in {
      packages.default = (self.overlays.default pkgs pkgs).t0yv0-emacs;

      # Needs a manual step to install on Mac OS. `nix build && cd result`, select and open all the
      # font files simultaneously, this prompts the installer.
      packages.iosevka = pkgs.iosevka;

      # On Macs need to copy these manually into ~/Library/Spelling for jinx to work.
      packages.dicts = pkgs.hunspellDicts.en_US;
    };

  in flake-utils.lib.eachDefaultSystem out // {
    overlays.default = overlay;
  };
}
