{
  pkgs,
  epkgs,
  stdenv,
  version,
  mermaid-cli,
  dape_src,
  mcpel_src,
  gptel_src,
  testrun ? pkgs.testrun,
  vterms ? pkgs.vterms,
  gh-autolinks ? pkgs.gh-autolinks,
  treesitedit ? pkgs.treesitedit,
  copilot ? pkgs.copilot-el,
  gptel-backend ? "anthropic",
  gptel-token-shell-command ? "",
}: let

treesitter = pkgs.tree-sitter.withPlugins (_: pkgs.tree-sitter.allGrammars);

gptel = epkgs.trivialBuild {
  pname = "gptel";
  version = "0.9.8";
  src = gptel_src;
  packageRequires = [
    epkgs.compat
    epkgs.transient
  ];
};

jsonrpc = epkgs.elpaBuild {
  pname = "jsonrpc";
  ename = "jsonrpc";
  version = "1.0.25";
  src = pkgs.fetchurl {
    url = "https://elpa.gnu.org/packages/jsonrpc-1.0.25.tar";
    sha256 = "sha256-zsBYbvpNWncnLpkUImgCzcJWhDaaiHloHEWNSjs4jEI=";
  };
  packageRequires = [];
  meta = {
    homepage = "https://elpa.gnu.org/packages/jsonrpc.html";
  };
};

eglot = epkgs.elpaBuild {
  pname = "eglot";
  ename = "eglot";
  version = "1.17";
  src = pkgs.fetchurl {
    url = "https://elpa.gnu.org/packages/eglot-1.17.tar";
    sha256 = "sha256-flJX2hEj34Ah9eeyhWz0RSYdvmwKPU5kA7bh0xBKVrE=";
  };
  packageRequires = [
    jsonrpc
  ];
  meta = {
    homepage = "https://elpa.gnu.org/packages/eglot.html";
  };
};

dape = epkgs.trivialBuild {
  pname = "dape";
  version = "0.0.1";
  src = dape_src;
  packageRequires = [jsonrpc];
};

mcpel = epkgs.trivialBuild {
  pname = "mcp.el";
  version = "0.0.1";
  src = mcpel_src;
  packageRequires = [];
};

t0yv0-basics = epkgs.trivialBuild {
  pname = "t0yv0-basics";
  version = "${version}";
  src = [ ./t0yv0-basics.el ];
  packageRequires = [
    epkgs.consult
    epkgs.dash
    epkgs.vterm
  ];
};

prebuilt = pkgs.symlinkJoin {
  name = "t0yv0-emacs-prebuilt-${version}";
  version = "${version}";
  paths = [
    epkgs.ace-window
    epkgs.avy # needed for ace-window
    epkgs.emacsql
    epkgs.embark
    epkgs.embark-consult
    epkgs.git-link
    epkgs.go-mode
    epkgs.haskell-mode
    epkgs.hcl-mode
    epkgs.htmlize
    epkgs.magit
    epkgs.magit-section
    epkgs.markdown-mode
    epkgs.mermaid-mode
    epkgs.minions
    epkgs.nix-mode
    epkgs.ob-go
    epkgs.org-roam
    epkgs.ormolu
    epkgs.pyvenv
    epkgs.rainbow-delimiters
    epkgs.reformatter
    epkgs.terraform-mode
    epkgs.transient
    epkgs.typescript-mode
    epkgs.with-editor
    epkgs.yaml-mode
    dape
    mcpel
    t0yv0-basics
  ];
};

bootstrap = stdenv.mkDerivation {
  name = "t0yv0-emacs-bootstrap-${version}";
  version = "${version}";
  builder = "${pkgs.bash}/bin/bash";
  coreutils = pkgs.coreutils;
  args = [
    ./bootstrap.sh "${prebuilt}" ./default.el
    "${treesitter}" "${mermaid-cli}/bin/mmdc" ./snippets
    "${gptel-backend}" "${gptel-token-shell-command}"
  ];
};

default-el = epkgs.trivialBuild {
  pname = "t0yv0-emacs-default-el";
  version = "${version}";
  src = [ "${bootstrap}" ];
  packageRequires = [
    t0yv0-basics
  ];
};

eager-packages = epkgs: [
  testrun
  vterms
  gh-autolinks
  treesitedit
  copilot
  eglot
  jsonrpc
  gptel
  default-el
  epkgs.corfu
  epkgs.diminish
  epkgs.doom-modeline
  epkgs.envrc
  epkgs.forge
  # Note on JINX: this spell-checking package requires additional packages; on NixOS
  # installing pkgs.nuspell and pkgs.hunspellDicts.en_US works fine; TBD on MacOS. I have not
  # taken the time to find how to inline this into t0yv0-emacs so it installs a local copy.
  epkgs.jinx
  epkgs.jupyter
  epkgs.marginalia
  epkgs.orderless
  epkgs.org-present
  epkgs.vertico
  epkgs.wgrep
  epkgs.yasnippet
];

in epkgs.emacsWithPackages eager-packages
