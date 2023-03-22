#!/bin/sh
protoc --plugin=`which protoc-gen-doc` compiler.proto --doc_out=../docs/ --doc_opt=markdown,compiler-api.md
echo "<!-- markdownlint-disable-file -->" >> ../docs/compiler-api.md
