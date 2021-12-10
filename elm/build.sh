#!/bin/sh
declare -a optimize
if [ "$1" == "-o" ]; then
  elm make src/Main.elm --output=dist/main.js --optimize
else
  elm make src/Main.elm --output=dist/main.js
fi
cp index.html dist/index.html
cp -R static/css dist
cp -R static/images dist