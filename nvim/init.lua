


-------------------------------------------
--- GENERAL OPTIONS
-------------------------------------------
vim.opt.number = true
vim.opt.relativenumber = false 
vim.opt.cursorline = true
vim.opt.mouse = 'a'
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = false -- highlight results of prev. search
vim.opt.wrap = true
vim.opt.breakindent = true -- preserve indentation of virtual lines
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab= false -- whether or not to turn Tab char into spaces
-------------------------------------------

-------------------------------------------
--- KEYMAPPERY
-------------------------------------------
vim.keymap.set('n', '§', ':', { remap=false, desc = '§ to : for easier cmd' })
vim.keymap.set('n', '¤', '$', { remap=false, desc = '¤ to $ for easier EOL jump' })
vim.keymap.set('n', '<C-t>', ':TagbarToggle<CR>', { remap=false, desc = 'Toggle Tagbar visibility' })
vim.keymap.set('n', '<C-f>', ':NERDTreeToggle<CR>', { remap=false, desc = 'Toggle NERDTree visibility' })
-------------------------------------------
--
-- END OF SETTINGS, (eventually)


local Plug = vim.fn['plug#']
vim.call('plug#begin', '~/.config/nvim/plugged')


-------------------------------------------
--- LOAD PLUGINS
-------------------------------------------

--- UTILITIES
Plug('scrooloose/nerdtree', {on = {'NERDTreeToggle', 'NERDTree'}})
Plug('junegunn/fzf', {['do'] = vim.fn['fzf#install']})
-- Plug('vim-airline/vim-airline')
Plug('nvim-lualine/lualine.nvim')
Plug('neoclide/coc.nvim', {branch = 'release'})
Plug('nvim-treesitter/nvim-treesitter', {['do'] = ':TSUpdate'})
Plug('preservim/tagbar')
-- gcc to toggle line comment (gc(motion))
Plug('tpope/vim-commentary')
Plug('tpope/vim-surround')


--- VISUALS/COLOURS
Plug('folke/tokyonight.nvim', {branch = 'main'})
Plug('ellisonleao/gruvbox.nvim')
Plug('kyazdani42/nvim-web-devicons')
-- Plug('vim-airline/vim-airline-themes')



vim.call('plug#end')
-------------------------------------------
--- END PLUGINS(, obviously :))


-------------------------------------------
--- LOAD CONFIGS				from ./lua/..
-------------------------------------------
require('lualine/config')


-------------------------------------------
--- SET THEME
-------------------------------------------
-- require('gruvbox').load()
require('tokyonight').load()
---------------------------------
