
-- EDITOR SETTINGS
-- 	Accessed through vim module's opt property
--
--	Options syntax: 
-- 		vim.opt.option_name = value
--
-- 	Keybinding syntax:
--		vim.keymap.set('n', '<space>w', '<cmd>write<cr>', {desc = 'Save'})
--		
--
--		src: https://vonheikemen.github.io/devlog/tools/build-your-first-lua-config-for-neovim/


-- Calling plugs in lua:
-- Here comes little bit of bad news. Plug has a couple of options that can cause an error, for and do. Those two are reserved keywords so we need use a different syntax when we use them
-- Plug('junegunn/goyo.vim', {['for'] = 'markdown'})

-- Now do is an interesting one. It takes a string or a function, and what's interesting is that we can give it a vim function or a lua function.
-- Plug('junegunn/fzf', {['do'] = vim.fn['fzf#install']})



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

vim.keymap.set('n', '§', ':', { remap=false, desc = '§ to : for easier cmd' })
vim.keymap.set('n', '¤', '$', { remap=false, desc = '¤ to $ for easier EOL jump' })
vim.keymap.set('n', '<C-t>', ':TagbarToggle<CR>', { remap=false, desc = 'Toggle Tagbar visibility' })
vim.keymap.set('n', '<C-f>', ':NERDTreeToggle<CR>', { remap=false, desc = 'Toggle NERDTree visibility' })


local Plug = vim.fn['plug#']
vim.call('plug#begin', '~/.config/nvim/plugged')

-- Plug 'tpope/vim-sensible'

--- UTILITIES
Plug('scrooloose/nerdtree', {on = {'NERDTreeToggle', 'NERDTree'}})
Plug('junegunn/fzf', {['do'] = vim.fn['fzf#install']})
Plug('vim-airline/vim-airline')
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
Plug('vim-airline/vim-airline-themes')



vim.call('plug#end')
