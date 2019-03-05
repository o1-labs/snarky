#!/bin/bash

set -e

rm -r website/website/pages/api || true
cp -r -T _build/default/_doc/_html/ website/website/pages/api

# Fix css and js URLs
pushd website/website/pages/api;
for filename in $(find -name '*.html'); do
  sed -e 's/\.\.\/odoc.css/..\/..\/..\/snarky\/css\/odoc.css/' -e 's/\.\.\/highlight.pack.js/..\/..\/..\/snarky\/js\/highlight.pack.js/' -i "$filename"
done
