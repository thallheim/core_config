#!/bin/sh

# Copies current awesome rc.lua to current dir and overwrites it.
# Copies from ~/.config/awesome/rc.lua

echo ":: Updating awesomewm configs..."
cp ~/.config/awesome/rc.lua ./awesome/rc.lua
echo ":: rc.lua copied."
echo ":: Done."
echo ":! Don't forget to push whatever you changed, yah?"
