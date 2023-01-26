{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-22.11;
  };

  outputs =
    { self,
      nixpkgs
    }:

    let package = sys:
          let
            pkgs = import nixpkgs { system = sys; };
          in
            import ./default.nix { pkgs = pkgs; };

    in {
      packages.x86_64-linux.default = package "x86_64-linux";
      packages.x86_64-darwin.default = package "x86_64-darwin";
    };
}
