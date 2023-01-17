require("mason").setup()
require("mason-lspconfig").setup{
		ensure_installed = { "denols", "eslint", "jsonls", "html", "astro", "emmet_ls" }
}
