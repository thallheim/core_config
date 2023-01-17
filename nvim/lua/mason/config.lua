require("mason").setup()
require("mason-lspconfig").setup{
		ensure_installed = { "denols", "eslint", "jsonls", "html", "vuels", "emmet_ls" }
}
