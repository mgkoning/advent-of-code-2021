#!/bin/sh
elm make src/Main.elm --output=dist/main.js
cp index.html dist/index.html
cp -R static/css dist
cp -R static/images dist