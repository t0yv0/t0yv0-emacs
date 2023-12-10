PATH=$coreutils/bin

version="$1"
echo "$version" >"$out/t0yv0-emacs-version.txt"

mkdir -p $out/bin

e="$out/bin/t0yv0-emacs"
echo "#!/usr/bin/env sh" >> $e
echo "export PATH=$emacs/bin:\$PATH" >> $e
echo "exec emacsclient -c --alternate-editor=" >> $e
chmod a+x $e


if [ -d "$emacs/Applications" ]; then
    mkdir "$out/Applications"
    cp -r "$emacs/Applications/Emacs.app" "$out/Applications/T0yv0Emacs.app"
fi
