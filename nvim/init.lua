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
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = false -- whether or not to turn Tab char into spaces
vim.opt.completeopt = {'menuone', 'noselect', 'noinsert'} --menuone=show when only one match | noselect=do not autoselect | noinsert=do not autoinsert
vim.opt.shortmess = vim.opt.shortmess + { c = true}
vim.api.nvim_set_option('updatetime', 300)
vim.cmd([[
set signcolumn=yes
autocmd CursorHold * lua vim.diagnostic.open_float(nil, { focusable = false })
]])
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
--- LSP
Plug('williamboman/mason.nvim')
Plug('williamboman/mason-lspconfig.nvim')
Plug('neovim/nvim-lspconfig')
Plug('simrat39/rust-tools.nvim')

--- COMPLETION & SNIPPETS
Plug('hrsh7th/cmp-buffer')
Plug('hrsh7th/cmp-nvim-lsp')
Plug('hrsh7th/cmp-nvim-lsp-signature-help')
Plug('hrsh7th/cmp-nvim-lua')
Plug('hrsh7th/cmp-path')
Plug('hrsh7th/cmp-vsnip')
Plug('hrsh7th/nvim-cmp')
Plug('hrsh7th/vim-vsnip')

-- DEBUGGING
-- Plug('nvim-lua/plenary.nvim')
-- Plug('mfussenegger/nvim-dap')

--- UTILITIES
Plug('scrooloose/nerdtree', {on = {'NERDTreeToggle', 'NERDTree'}})
Plug('junegunn/fzf', {['do'] = vim.fn['fzf#install']})
Plug('vim-airline/vim-airline')
Plug('nvim-treesitter/nvim-treesitter', {['do'] = ':TSUpdate'})
Plug('preservim/tagbar')
Plug('tpope/vim-commentary') -- gcc to toggle line comment (gc(motion))
Plug('tpope/vim-surround')
Plug('romgrk/barbar.nvim')

--- VISUALS/COLOURS
Plug('folke/tokyonight.nvim', {branch = 'main'})
Plug('ellisonleao/gruvbox.nvim')
Plug('kyazdani42/nvim-web-devicons')
Plug('tanvirtin/monokai.nvim')
-- Plug('nvim-lualine/lualine.nvim')

vim.call('plug#end')
-------------------------------------------
--- END PLUGINS(, obviously :))


-------------------------------------------
--- LOAD CONFIGS				from ./lua/..
-------------------------------------------
-- require('lualine/config') 								-- Something's fucky. 
require('mason/config')
require('lsp/config')
require('completion/config')
require('treesitter/config')
require('barbar/keymap')

-------------------------------------------
--- RUST-TOOLS SETTINGS 		(rust-analyzer)
-------------------------------------------
require('rust-tools').inlay_hints.enable() 	-- Set inlay hints: all buffers.
-- require('lsp/config').inlay_hints.set() 	-- Set inlay hints: curr. buffer.


-------------------------------------------
--- DENO LSP SETUP
-------------------------------------------
vim.g.markdown_fenced_languages = {
  "ts=typescript"
}
require('lspconfig').denols.setup{}

-------------------------------------------
--- LSP CONFIG (Diagnostics Options Setup)
-------------------------------------------
local sign = function(opts)
  vim.fn.sign_define(opts.name, {
    texthl = opts.name,
    text = opts.text,
    numhl = ''
  })
end
-------------------------------------------

-------------------------------------------
--- COMPLETION CONFIG 
-------------------------------------------
-------------------------------------------
--- SET THEME
-------------------------------------------
-- require('gruvbox').load()
-- require('tokyonight').load()
-- require('monokai').setup {}
require('monokai').setup { palette = require('monokai').pro }
---------------------------------
