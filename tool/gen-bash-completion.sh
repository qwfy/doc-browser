#!/usr/bin/env bash

stack exec doc-browser -- --bash-completion-script '`which doc-browser`' > doc-browser.bash-completion
