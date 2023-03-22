demo:
	rm -fr .work-dir
	mkdir .work-dir
	lbf compile --debug --compiler `which lbc` -w .work-dir -i lbf-base -i api/ -f api/Demo.lbf
	lbg gen-purescript --debug -i .work-dir/compiler-input.textproto -o .work-dir/codegen-output.textproto -p autogen

coop:
	rm -fr .work-dir
	mkdir .work-dir
	lbf compile --debug --compiler `which lbc` -w .work-dir -i lbf-base -i api/ -f api/Coop.lbf
	lbg gen-purescript --debug -i .work-dir/compiler-input.textproto -o .work-dir/codegen-output.textproto -p autogen
