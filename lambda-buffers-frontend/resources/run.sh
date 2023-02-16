function lbf {
   cabal run lbf -- $@
}

function lbf-comp {
    lbf compile --compiler `which lbc` --debug $@
}

lbf-comp -i duplicate_tydef -f duplicate_tydef/A.lbf
lbf-comp -i import_cycle_found -f import_cycle_found/A.lbf
lbf-comp -i imported_not_found -f imported_not_found/A.lbf
lbf-comp -i invalid_module_filepath -f invalid_module_filepath/A.lbf
lbf-comp -i module_not_found -f module_not_found/A.lbf
lbf-comp -i module_parse_error -f module_parse_error/A.lbf
lbf-comp -i multiple_modules_found -i multiple_modules_found/another_import_path -f multiple_modules_found/A.lbf
lbf-comp -i symbol_already_imported -f symbol_already_imported/A.lbf
lbf-comp -i tydef_name_conflict -f tydef_name_conflict/A.lbf
lbf-comp -i tyref_not_found -f tyref_not_found/A.lbf

lbf-comp -w good/work-dir -i good -f good/Test.lbf
