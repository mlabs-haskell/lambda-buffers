#!/bin/bash
function lbf {
    cabal run lbf -- $@
}

function lbf-build {
    lbf build --compiler `which lbc` --debug $@
}

function lbf-form {
    lbf format -i -f $@
}

lbf-build -i duplicate_tydef -f duplicate_tydef/A.lbf
lbf-build -i import_cycle_found -f import_cycle_found/A.lbf
lbf-build -i imported_not_found -f imported_not_found/A.lbf
lbf-build -i invalid_module_filepath -f invalid_module_filepath/A.lbf
lbf-build -i module_not_found -f module_not_found/A.lbf
lbf-build -i module_parse_error -f module_parse_error/A.lbf
lbf-build -i multiple_modules_found -i multiple_modules_found/another_import_path -f multiple_modules_found/A.lbf
lbf-build -i symbol_already_imported -f symbol_already_imported/A.lbf
lbf-build -i tydef_name_conflict -f tydef_name_conflict/A.lbf
lbf-build -i tyref_not_found -f tyref_not_found/A.lbf

lbf-build -w good/work-dir -i good -f good/Test.lbf
lbf-form good/Test.lbf
lbf-build -w good/work-dir -i good -f good/Test.lbf

echo "goldens/good/Prelude.lbf"
lbf-build -w goldens/good/work-dir -i goldens/good -f goldens/good/Prelude.lbf
lbf-form goldens/good/Prelude.lbf
lbf-build -w goldens/good/work-dir -i goldens/good -f goldens/good/Prelude.lbf

echo "goldens/good/PreludeT.lbf"
lbf-build -w goldens/good/work-dir -i goldens/good -f goldens/good/PreludeT.lbf
lbf-form goldens/good/PreludeT.lbf
lbf-build -w goldens/good/work-dir -i goldens/good -f goldens/good/PreludeT.lbf

echo "goldens/good/Plutus.lbf"
lbf-build -w goldens/good/work-dir -i goldens/good -f goldens/good/Plutus.lbf

echo "goldens/good/Plutus/V1.lbf"
lbf-build -w goldens/good/work-dir -i goldens/good -f goldens/good/Plutus/V1.lbf

echo "goldens/good/Plutus/V2.lbf"
lbf-build -w goldens/good/work-dir -i goldens/good -f goldens/good/Plutus/V2.lbf

echo "goldens/good/Coop.lbf"
lbf-build -w goldens/good/work-dir -i goldens/good -f goldens/good/Coop.lbf

echo "goldens/good/Rules.lbf"
lbf-build -w goldens/good/work-dir -i goldens/good -f goldens/good/Rules.lbf
lbf-form goldens/good/Rules.lbf
lbf-build -w goldens/good/work-dir -i goldens/good -f goldens/good/Rules.lbf

echo "goldens/good/LambdaBuffers.lbf"
lbf-build -w goldens/good/work-dir -i goldens/good -f goldens/good/LambdaBuffers.lbf
lbf-form goldens/good/LambdaBuffers.lbf
lbf-build -w goldens/good/work-dir -i goldens/good -f goldens/good/LambdaBuffers.lbf

# find goldens/ -name "*.lbf" | entr -p cabal run lbf -- compile --compiler `which lbc` --debug -w goldens/good/work-dir -i goldens/good -f /_
