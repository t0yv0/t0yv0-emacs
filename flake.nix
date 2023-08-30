{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs?rev=b7cde1c47b7316f6138a2b36ef6627f3d16d645c;
  };

  outputs = { self, nixpkgs }:
    let

      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      packages = sys:
        let
          pkgs = import nixpkgs { system = sys; };
        in {
          default = import ./default.nix { pkgs = pkgs; };
          default-el = import ./default-el.nix { pkgs = pkgs; };
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
