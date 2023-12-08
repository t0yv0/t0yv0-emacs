{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs?rev=b7cde1c47b7316f6138a2b36ef6627f3d16d645c;
    nixpkgs_22_11.url = github:NixOS/nixpkgs/22.11;
  };

  outputs = { self, nixpkgs, nixpkgs_22_11 }: let

    packages = sys: let
      pkgs = import nixpkgs { system = sys; };
      pkgs_22_11 = import nixpkgs_22_11 { system = sys; };
      software = import ./default.nix {
        pkgs = pkgs;
        pkgs_22_11 = pkgs_22_11;
        version = self.rev or "dirty";
      };
    in {
      default = software.t0yv0-emacs;
      copilot = software.copilot;
      mermaid = software.mermaid;
      t0yv0-emacs = software.t0yv0-emacs;
      t0yv0-emacs-lisp = software.t0yv0-emacs-lisp;
      t0yv0-ware = software.t0yv0-ware;
      treesitter = software.treesitter;
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
