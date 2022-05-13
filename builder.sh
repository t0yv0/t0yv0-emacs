PATH=$coreutils/bin
mkdir -p $out/bin
e="$out/bin/t0yv0-emacs"
echo "#!/usr/bin/env sh" >> $e
echo "export PATH=$emacs/bin:\$PATH" >> $e
if [ -d "$emacs/Applications" ]; then
    mkdir "$out/Applications"
    cp -r "$emacs/Applications/Emacs.app" "$out/Applications/T0yv0Emacs.app"

    # BEGIN rewrite MacOS entrypoint to add Nix PATH entries
    entry="$out/Applications/T0yv0Emacs.app/Contents/MacOS/Emacs"
    fixed="./Emacs-fixed"

    head -n 1 "$entry" >> "$fixed"
    echo "" >> "$fixed"
    echo "export PATH=\$HOME/bin:\$HOME/.nix-profile/bin:\$PATH" >> "$fixed"
    echo "" >> "$fixed"
    tail -n+2 "$entry" >> "$fixed"

    chmod a+w "$entry"
    cat "$fixed" > "$entry"
    rm "$fixed"
    chmod a-w "$entry"
    # END
fi
echo "exec emacsclient -c --alternate-editor=" >> $e
chmod a+x $e
