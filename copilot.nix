{ pkgs ? import <nixpkgs>{} }:

pkgs.emacsPackages.trivialBuild {
  pname = "copilot";
  src = pkgs.fetchFromGitHub {
    owner = "zerolfx";
    repo = "copilot.el";
    rev = "4f83577b3a3c120e9b1063b5a793c20a6ed9ece0";
    sha256 = "sha256-TokWEMyWcqPU1V0FX7faxbl5gUBXf8DoqFeXHwIKP8Y=";
  };
  packageRequires = [
    pkgs.emacsPackages.dash
    pkgs.emacsPackages.s
    pkgs.emacsPackages.editorconfig
  ];
  postBuild = ''
    mkdir -p $out/share/emacs/site-lisp
    cp -r $src/dist $out/share/emacs/site-lisp/
  '';
}
