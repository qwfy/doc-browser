#!/usr/bin/bash

confighome=~/temp/home/xdg-config
cachehome=~/temp/home/xdg-cache

mkdir -p $confighome
mkdir -p $cachehome

export XDG_CONFIG_HOME=$confighome
export XDG_CACHE_HOME=$cachehome

stack exec doc-browser "$@"
