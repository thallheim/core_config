#!/bin/sh

# Copies system's emacs config to current dir and overwrites it.
# Copies from ~/.emacs and ~/.emacs.d

echo ":: Updating emacs config..."
cp ~/.emacs ./emacs/.emacs
echo ":: .emacs copied."
cp -r ~/.emacs.d/ ./emacs/
echo ":: config subfolder copied."
echo ":: Done."
echo ":! Don't forget to push whatever you changed, yah?"
