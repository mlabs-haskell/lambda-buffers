#!/bin/bash
function lbg {
    cabal run lbg -- $@
}

lbg gen-haskell -i lambda-buffers.input.textproto -o lambda-buffers.output.textproto -p haskell-autogen

lbg gen-purescript -i lambda-buffers.input.textproto -o lambda-buffers.output.textproto -p purescript-autogen

# ghci   -hide-all-packages -Wmissing-home-modules   -no-user-package-db   -package-db /home/bladyjoker/.cabal/store/ghc-9.2.3/package.db   -package-db /home/bladyjoker/Desktop/cardano-open-oracle-protocol/coop-plutus/dist-newstyle/packagedb/ghc-9.2.3   -package-db /home/bladyjoker/Desktop/cardano-open-oracle-protocol/coop-plutus/dist-newstyle/build/x86_64-linux/ghc-9.2.3/coop-plutus-0.1.0.0/package.conf.inplace   -package-id base-4.16.2.0   -package-id bytestring-0.11.3.1   -package-id text-1.2.5.0   -package-id containers-0.6.5.1   -package-id plutus-ledger-api-1.0.0.0-5I44wBlQkJcAkvH0pssHIv   -package-id plutus-tx-1.0.0.0-36ltsZtQ7xF5Mr0iGlXK1I   -XHaskell2010   -i/home/bladyjoker/Desktop/lambda-buffers/lambda-buffers-codegen/data/goldens/autogen   -i/home/bladyjoker/Desktop/lambda-buffers/lambda-buffers-codegen/data/goldens/runtime  ../../lambda-buffers/lambda-buffers-codegen/data/goldens/autogen/LambdaBuffers/Coop.hs
