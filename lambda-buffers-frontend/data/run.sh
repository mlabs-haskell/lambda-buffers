#!/bin/bash
function lbf {
    cabal run lbf -- $@
}

function lbf-build {
    DIR=$1
    lbf build --compiler `which lbc` --gen `which lbg-haskell` --debug --import-path $1 --gen-dir "$1/autogen" "$1/$2"
}

function lbf-build2 {
    DIR=$1
    lbf build --compiler `which lbc` --gen `which lbg-haskell` --debug --import-path $1 --gen-dir "$1/autogen" "$1/$2"
}

function lbf-form {
    lbf format -i -f $@
}

lbf-build duplicate_tydef A.lbf
lbf-build import_cycle_found A.lbf
lbf-build imported_not_found A.lbf
lbf-build invalid_module_filepath A.lbf
lbf-build module_not_found A.lbf
lbf-build module_parse_error A.lbf
lbf build --compiler `which lbc` --gen `which lbg-haskell` --debug \
    --import-path multiple_modules_found \
    --import-path multiple_modules_found/another_import_path \
    --gen-dir multiple_modules_found/autogen \
    multiple_modules_found/A.lbf
lbf-build symbol_already_imported A.lbf
lbf-build tydef_name_conflict A.lbf
lbf-build tyref_not_found A.lbf

lbf-build good Test.lbf
lbf-form good/Test.lbf
lbf-build good Test.lbf

echo "goldens/good/Prelude.lbf"
lbf-build goldens/good Prelude.lbf
lbf-form goldens/good/Prelude.lbf
lbf-build goldens/good Prelude.lbf

echo "goldens/good/PreludeT.lbf"
lbf-build goldens/good PreludeT.lbf
lbf-form goldens/good/PreludeT.lbf
lbf-build goldens/good PreludeT.lbf

echo "goldens/good/Plutus.lbf"
lbf-build goldens/good Plutus.lbf

echo "goldens/good/Plutus/V1.lbf"
lbf-build goldens/good Plutus/V1.lbf

echo "goldens/good/Plutus/V2.lbf"
lbf-build goldens/good Plutus/V2.lbf

echo "goldens/good/Coop.lbf"
lbf-build goldens/good Coop.lbf

echo "goldens/good/Rules.lbf"
lbf-build goldens/good Rules.lbf
lbf-form goldens/good/Rules.lbf
lbf-build goldens/good Rules.lbf

echo "goldens/good/LambdaBuffers.lbf"
lbf-build goldens/good LambdaBuffers.lbf
lbf-form goldens/good/LambdaBuffers.lbf
lbf-build goldens/good LambdaBuffers.lbf

# find goldens/ -name "*.lbf" | entr -p cabal run lbf -- compile --compiler `which lbc` --debug goldens/good /_
