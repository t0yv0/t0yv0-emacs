# To make builds reproducible, we fix several version of nixpkgs. The
# most conservative default is using the last tagged version, aliased
# simply pkgs. For various language stacks we need more bleeding age.

let

  pkgs-mar-28-2022 = import (builtins.fetchTarball {
      name = "pkgs-mar-28-2022";
      url = "https://github.com/nixos/nixpkgs/archive/80d8655c15055472f0118a877351085cc22c1e92.tar.gz";
      sha256 = "14aq8rikhpiwv7aw15cjxg88vxwzg02gn4w3wxqnm4g3yy20zzik";
  }) {};

in {
  pkgs = pkgs-mar-28-2022;
}
