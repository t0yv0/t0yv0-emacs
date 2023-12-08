{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs?rev=b7cde1c47b7316f6138a2b36ef6627f3d16d645c;
    nixpkgs_22_11.url = github:NixOS/nixpkgs/22.11;
  };

  outputs = { self, nixpkgs, nixpkgs_22_11 }: let
    systems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ];

    packages = sys: let
      pkgs = import nixpkgs { system = sys; };
      pkgs_22_11 = import nixpkgs_22_11 { system = sys; };
    in {
      default = import ./default.nix {
        pkgs = pkgs;
        pkgs_22_11 = pkgs_22_11;
        version = self.rev or "dirty";
      };
      t0yv0-ware = import ./t0yv0-ware.nix { pkgs = pkgs; };
      copilot = import ./copilot.nix { pkgs = pkgs; };
      ts = import ./ts.nix { pkgs = pkgs; };
    };

    pkgsMap = builtins.listToAttrs (builtins.map (sys: {
      name = sys;
      value = packages sys;
    }) systems);

  in {
    packages = pkgsMap;
  };
}
