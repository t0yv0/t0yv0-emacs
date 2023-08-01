{
  pkgs ? import <nixpkgs>{},
}:

pkgs.tree-sitter.withPlugins (_: pkgs.tree-sitter.allGrammars)
