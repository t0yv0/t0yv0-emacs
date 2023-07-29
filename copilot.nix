{ pkgs ? import <nixpkgs>{},
  epkgs ? pkgs.emacsPackages,
  src ? pkgs.fetchFromGitHub {
    owner = "zerolfx";
    repo = "copilot.el";
    rev = "4f83577b3a3c120e9b1063b5a793c20a6ed9ece0";
    sha256 = "sha256-TokWEMyWcqPU1V0FX7faxbl5gUBXf8DoqFeXHwIKP8Y=";
  }
}:

epkgs.trivialBuild {
  pname = "copilot";
  src = src;
  packageRequires = [
    epkgs.dash
    epkgs.s
    epkgs.editorconfig
  ];
  postBuild = ''
    mkdir -p $out/share/emacs/site-lisp
    cp -r $src/dist $out/share/emacs/site-lisp/
  '';
}
