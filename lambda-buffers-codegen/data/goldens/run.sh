#!/bin/bash
function lbg {
    cabal run lbg -- $@
}

lbg gen-haskell -i lambda-buffers.input.textproto -o lambda-buffers.output.textproto -p autogen
