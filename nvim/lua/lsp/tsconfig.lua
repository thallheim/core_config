on_attach = function(client, bufnr)
	local opts = { noremap = true, silent = true }
	-- Hover actions
	vim.api.nvim_buf_set_keymap('n', '*', '<cmd>lua vim.lsp.buf.hover()<CR>', { buffer = bufnr })
end
