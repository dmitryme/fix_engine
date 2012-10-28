#!/bin/bash

git submodule init
git submodule update
mkdir -p .build
cd .build
cmake ../fix_parser
make
