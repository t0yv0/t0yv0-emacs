PATH=$coreutils/bin
mkdir -p $out/bin
e="$out/bin/t0yv0-emacs"
echo "#!/usr/bin/env sh" >> $e
echo "export PATH=$emacs/bin:\$PATH" >> $e
echo "exec emacsclient -c --alternate-editor=" >> $e
chmod a+x $e
