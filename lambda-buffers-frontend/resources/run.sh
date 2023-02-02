function lbcli {
   cabal run lambda-buffers-frontend-cli -- $@
}

function lbcompile {
    lbcli compile --compiler `which lambda-buffers-compiler-cli` --debug $@
}

lbcompile -i duplicate_tydef -f duplicate_tydef/A.lbf
lbcompile -i import_cycle_found -f import_cycle_found/A.lbf
lbcompile -i imported_not_found -f imported_not_found/A.lbf
lbcompile -i invalid_module_filepath -f invalid_module_filepath/A.lbf
lbcompile -i module_not_found -f module_not_found/A.lbf
lbcompile -i module_parse_error -f module_parse_error/A.lbf
lbcompile -i multiple_modules_found -i multiple_modules_found/another_import_path -f multiple_modules_found/A.lbf
lbcompile -i symbol_already_imported -f symbol_already_imported/A.lbf
lbcompile -i tydef_name_conflict -f tydef_name_conflict/A.lbf
lbcompile -i tyref_not_found -f tyref_not_found/A.lbf

lbcompile -i good -f good/Test.lbf
