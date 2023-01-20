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
vim.opt.expandtab = true -- whether or not to turn Tab char into spaces
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
vim.keymap.set('n', 'å', '<cmd>lua vim.lsp.buf.hover()<CR>', { remap=false, silent= false, desc = 'Show docs for obj under cursor' })
vim.keymap.set('n', '<C-å>', '<cmd>lua vim.lsp.buf.hover()', { remap=false, silent= false, desc = 'Show docs for obj under cursor' })

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
Plug('chrisgrieser/cmp-nerdfont')

-- DEBUGGING
-- Plug('nvim-lua/plenary.nvim')
-- Plug('mfussenegger/nvim-dap')

--- UTILITIES
Plug('scrooloose/nerdtree', {on = {'NERDTreeToggle', 'NERDTree'}})
Plug('junegunn/fzf', {['do'] = vim.fn['fzf#install']})
-- Plug('vim-airline/vim-airline')
-- Plug('vim-airline/vim-airline-themes')
Plug('nvim-lualine/lualine.nvim')
Plug('nvim-treesitter/nvim-treesitter', {['do'] = ':TSUpdate'})
Plug('preservim/tagbar')
Plug('tpope/vim-commentary') -- gcc to toggle line comment (gc(motion))
Plug('tpope/vim-surround')
Plug('romgrk/barbar.nvim')
Plug('mg979/vim-visual-multi')

--- VISUALS/COLOURS
Plug('folke/tokyonight.nvim', {branch = 'main'})
Plug('ellisonleao/gruvbox.nvim')
Plug('kyazdani42/nvim-web-devicons')
Plug('tanvirtin/monokai.nvim')
Plug('Shatur/neovim-ayu')
Plug('RRethy/nvim-base16')

vim.call('plug#end')
-------------------------------------------
--- END PLUGINS(, obviously :))


-------------------------------------------
--- LOAD CONFIGS				from ./lua/..
-------------------------------------------
require('lualine/config') 								-- Something's fucky. 
require('mason/config')
require('lsp/rusttools')
require('completion/config')
require('treesitter/config')
require('barbar/keymap')

-------------------------------------------
--- RUST-TOOLS SETTINGS 		(rust-analyzer)
-------------------------------------------
require('rust-tools').inlay_hints.enable() 	-- Set inlay hints: all buffers.
-- require('lsp/config').inlay_hints.set() 	-- Set inlay hints: curr. buffer.


-------------------------------------------
--- LSP LOAD
-------------------------------------------
vim.g.markdown_fenced_languages = {
  "ts=typescript"
}
require('lspconfig').denols.setup{
	filetypes = { ".ts", ".tsx" }
}
require('lspconfig').tsserver.setup{
	filetypes = { "javascript", "javascript.jsx", "javascriptreact" },
	hostInfo = "neovim"
}
-- Enable (broadcasting) snippet capability for completion
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true
require('lspconfig').jsonls.setup{
	capabilities = capabilities,
}
-- require('lspconfig').vuels.setup{}
require('lspconfig').volar.setup{}
require('lspconfig').html.setup{}
require('lspconfig').astro.setup{}
require('lspconfig').cssls.setup{}
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
--- LOAD LUALINE 
require('lualine').setup{
    options = { theme = 'auto' }
}
-------------------------------------------
-------------------------------------------
--- SET THEME
-------------------------------------------
-- require('gruvbox').load()
-- require('tokyonight').load()
-- require('monokai').setup {}
-- require('monokai').setup { palette = require('monokai').pro }
-- require('monokai').setup { palette = require('monokai').ristretto}
vim.cmd('colorscheme base16-tokyo-city-terminal-dark')
---------------------------------
