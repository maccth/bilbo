#!/bin/sh

# Script for cleaning all bin and obj folders, previously causing errors
# Will be done with FAKE in future
# Source: https://github.com/ionide/ionide-vscode-fsharp/issues/924

DIRS="src/Parser test/ParserTests"
for dir in $DIRS; do
	rm -rf $dir/bin $dir/obj
done
for dir in $DIRS; do
	dotnet build $dir
done