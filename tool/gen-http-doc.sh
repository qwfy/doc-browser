#!/usr/bin/env bash

tmpfile=/tmp/doc-browser.md
stack exec doc-browser -- --print-api > $tmpfile
pandoc -f markdown -t html $tmpfile > http-interface.html
trash $tmpfile
