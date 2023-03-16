#!/bin/bash
function lbg {
    cabal run lbg -- $@
}

lbg gen-haskell -i lambda-buffers.input.textproto -o lambda-buffers.output.textproto -p autogen

prefix="{- ORMOLU_DISABLE -}\n{- HLINT ignore -}\n"
# https://stackoverflow.com/questions/49778741/add-a-new-line-of-text-at-the-top-of-a-file-in-bash-shell
find . -name "*.hs" | while read f;do
                            mv $f "$f.tmp"
                            echo -ne $prefix > $f;
                            cat "$f.tmp" >> $f;
                            rm "$f.tmp";
                      done
