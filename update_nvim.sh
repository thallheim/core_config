#!/bin/sh

# Copies current nvim init.lua to current dir and overwrites it.
# Copies from ~/.config/nvim/init.lua

echo ":: Updating nvim configs..."
cp ~/.config/nvim/init.lua ./nvim/init.lua
echo ":: init.lua copied."
cp -r ~/.config/nvim/lua/ ./nvim/
echo ":: config subfolder copied."
echo ":: Done."
echo ":! Don't forget to push whatever you changed, yah?"
