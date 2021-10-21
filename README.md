# t0yv0-emacs

Emacs has been my daily driver at work during 2017-2021, and I now use
it at home also. I found that wrapping configuration in a
[Nix](https://github.com/NixOS/nix) derivation is really convenient to
reproduce the exact config on my Mac and Linux machines.


## Installation

To install in your Nix environment:

```
nix-env -i -f default.nix
```

You can also reference a pinned version declaratively in a Nix file,
for example:


```
{ pkgs ? import <nixpkgs>{} }:

let

  t0yv0-emacs = import (builtins.fetchTarball {
    name = "t0yv0-emacs-latest";
    url = https://github.com/t0yv0/t0yv0-emacs/archive/bf55303df26c7d4b7d66cf349866ae1f32e603a0.tar.gz;
    # Hash obtained using `nix-prefetch-url --unpack <url>`
    sha256 = "0fpnyv69yd9zh64flncf6c233kydvbrg7251aj3p3kbf2sl1y0aj";
  });

in

t0yv0-emacs {
  pkgs = pkgs;
}
```


## Use

```
t0yv0-emacs
```
