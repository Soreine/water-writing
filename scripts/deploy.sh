#!/bin/bash
set -euo pipefail
IFS=$'\n\t'


# Build
elm-make main.elm --output main.js

# Deploy
cp index.html style.css main.js dist

git add -f dist/
git ci -m 'Deploy gh-pages'

git subtree push --prefix dist origin gh-pages
