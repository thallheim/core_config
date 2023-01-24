#!/bin/sh

# Copies user's tmux config to current dir and overwrites it.
# Copies from ~/.tmux and ~/.tmux.conf

echo ":: Updating tmux config..."
cp ~/.tmux.conf ./tmux/.tmux.conf
echo ":: .tmux.conf copied."
echo ":! Don't forget to push whatever you changed, yah?"
