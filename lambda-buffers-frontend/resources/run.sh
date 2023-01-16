function lbcli {
   cabal run lambda-buffers-frontend-cli -- $@
}

lbcli compile -i duplicate_tydef -f duplicate_tydef/A.lbf
lbcli compile -i import_cycle_found -f import_cycle_found/A.lbf
lbcli compile -i imported_not_found -f imported_not_found/A.lbf
lbcli compile -i invalid_module_filepath -f invalid_module_filepath/A.lbf
lbcli compile -i module_not_found -f module_not_found/A.lbf
lbcli compile -i module_parse_error -f module_parse_error/A.lbf
lbcli compile -i multiple_modules_found -i multiple_modules_found/another_import_path -f multiple_modules_found/A.lbf
lbcli compile -i symbol_already_imported -f symbol_already_imported/A.lbf
lbcli compile -i tydef_name_conflict -f tydef_name_conflict/A.lbf
lbcli compile -i tyref_not_found -f tyref_not_found/A.lbf

lbcli compile -i good -f good/Test.lbf
