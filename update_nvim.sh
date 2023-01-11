!#bin/bash

# Copies current nvim init.lua to current dir and overwrites it.
# Copies from ~/.config/nvim/init.lua

echo ":: Updating nvim config..."
cp ~/.config/nvim/init.lua ./init.lua
echo ":: init.lua copied."
echo ":! Don't forget to push whatever you changed, yah?"
