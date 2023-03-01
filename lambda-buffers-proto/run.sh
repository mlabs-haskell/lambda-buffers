#!/bin/sh
protoc --plugin=`which protoc-gen-doc` compiler.proto --doc_out=. --doc_opt=markdown,compiler-proto.md
echo "<!-- markdownlint-disable-file -->" >> compiler-proto.md
