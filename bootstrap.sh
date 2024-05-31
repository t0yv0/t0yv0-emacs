PATH=$coreutils/bin

mkdir -p $out

treesitter="$3"
treesitter_out="$out/share/emacs/site-lisp/tree-sitter"

mmdc="$4"
mmdc_out="$out/share/emacs/site-lisp/mermaid"

snippets_out="$out/share/emacs/site-lisp/snippets"

mkdir -p "$treesitter_out"
mkdir -p "$mmdc_out"

cat << EOF > $out/default.el
(defvar t0yv0/treesitter-dir "$treesitter_out")
(defvar t0yv0/mermaid-mmdc-location "$mmdc_out/mmdc")
(defvar t0yv0/yas-snippets "$snippets_out")

(let* ((d "$1")
       (default-directory (concat d "/share/emacs/site-lisp")))
  (setq load-path (cons default-directory load-path))
  (normal-top-level-add-subdirs-to-load-path)
  (when (featurep 'native-compile)
    (setq native-comp-eln-load-path
          (append (list (car native-comp-eln-load-path))
                (mapcar (lambda (profile-dir)
                          (concat profile-dir "/share/emacs/native-lisp/"))
                        (list d))
                (cdr native-comp-eln-load-path)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

EOF

cat "$2" >> $out/default.el

mkdir -p $out/share/emacs/site-lisp/tree-sitter
ln -s ${treesitter}/go.so ${treesitter_out}/libtree-sitter-go.so
ln -s ${treesitter}/gomod.so ${treesitter_out}/libtree-sitter-gomod.so
ln -s ${treesitter}/typescript.so ${treesitter_out}/libtree-sitter-typescript.so
ln -s ${treesitter}/python.so ${treesitter_out}/libtree-sitter-python.so
ln -s ${treesitter}/tsx.so ${treesitter_out}/libtree-sitter-tsx.so

ln -s ${mmdc} ${mmdc_out}/mmdc

snippets="$5"
cp -r ${snippets} "${snippets_out}"
