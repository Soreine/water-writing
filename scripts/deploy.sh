#!/bin/bash
set -euo pipefail
IFS=$'\n\t'


# Build
elm-make main.elm --output main.js

# Deploy
git subtree push --prefix dist origin gh-pages
