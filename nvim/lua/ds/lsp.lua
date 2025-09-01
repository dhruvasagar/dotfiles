-- LSP settings.
--  This function gets run when an LSP connects to a particular buffer.
local M = {}

-- local on_attach = function(client, bufnr)
-- 	vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc", { buf = bufnr })
--
-- 	-- NOTE: Remember that lua is a real programming language, and as such it is possible
-- 	-- to define small helper and utility functions so you don't have to repeat yourself
-- 	-- many times.
-- 	--
-- 	-- In this case, we create a function that lets us more easily define mappings specific
-- 	-- for LSP related items. It sets the mode, buffer and description for us each time.
-- 	local nmap = function(keys, func, desc)
-- 		if desc then
-- 			desc = "LSP: " .. desc
-- 		end
--
-- 		vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
-- 	end
--
-- 	nmap("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")
-- 	nmap("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction")
--
-- 	nmap("gd", require("telescope.builtin").lsp_definitions, "[G]oto [D]efinition")
-- 	nmap("gr", require("telescope.builtin").lsp_references, "[G]oto [R]eferences")
-- 	nmap("gI", require("telescope.builtin").lsp_implementations, "[G]oto [I]mplementation")
-- 	-- nmap("gI", require("snacks").picker.lsp_implementations, "[G]oto [I]mplementation")
-- 	nmap("<leader>D", require("telescope.builtin").lsp_type_definitions, "Type [D]efinition")
-- 	nmap("<leader>ds", require("telescope.builtin").lsp_document_symbols, "[D]ocument [S]ymbols")
-- 	nmap("<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")
-- 	nmap("<leader>lr", vim.lsp.codelens.run, "[R]un [C]odelens")
-- 	nmap("<Leader>ih", function()
-- 		vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
-- 	end, "[I]nlay [H]ints")
--
-- 	-- See `:help K` for why this keymap
-- 	nmap("K", vim.lsp.buf.hover, "Hover Documentation")
-- 	nmap("<C-k>", vim.lsp.buf.signature_help, "Signature Documentation")
--
-- 	-- Lesser used LSP functionality
-- 	nmap("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")
-- 	nmap("<leader>wa", vim.lsp.buf.add_workspace_folder, "[W]orkspace [A]dd Folder")
-- 	nmap("<leader>wr", vim.lsp.buf.remove_workspace_folder, "[W]orkspace [R]emove Folder")
-- 	nmap("<leader>wl", function()
-- 		print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
-- 	end, "[W]orkspace [L]ist Folders")
--
-- 	-- Diagnostics
-- 	nmap("gl", vim.diagnostic.open_float, "[O]pen [D]iagnostics")
-- 	nmap("[d", vim.diagnostic.jump({ count = -1 }), "[G]oto [P]revious Diagnostics")
-- 	nmap("]d", vim.diagnostic.jump({ count = 1 }), "[G]oto [N]ext Diagnostics")
--
-- 	vim.api.nvim_create_autocmd("LspAttach", {
-- 		group = vim.api.nvim_create_augroup("CustomLspAttach", {}),
-- 		callback = function(evn)
-- 			local client = vim.lsp.get_client_by_id(ev.data.client_id)
-- 			on_attach(client, bufnr())
-- 		end,
-- 	})
--
-- 	-- vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI", "InsertLeave" }, {
-- 	--   group = vim.api.nvim_create_augroup("LspCodelens", {}),
-- 	--   callback = function()
-- 	--     if client.server_capabilities.codeLensProvider and client.supports_method("textDocument/codeLens") then
-- 	--       pcall(vim.lsp.codelens.refresh, { bufnr = 0 })
-- 	--     end
-- 	--   end,
-- 	-- })
--
-- 	if client.server_capabilities.inlayHintProvider then
-- 		vim.lsp.inlay_hint.enable(true)
-- 	end
-- end
--
-- M.on_attach = on_attach

local servers = {
	"angularls",
	"clojure_lsp",
	"ts_ls",
	"dockerls",
	"eslint",
	"gopls",
	"gradle_ls",
	"hls",
	"texlab",
	"jsonls",
	"marksman",
	"rust_analyzer",
	"vimls",
	"terraformls",
	"yamlls",
	"basedpyright",
	"lua_ls",
	"jdtls",
}

vim.lsp.enable(servers)

require("java").setup({
	spring_boot_tools = {
		enabled = false,
	},
})

require("mason").setup()

--
-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
-- local capabilities = vim.lsp.protocol.make_client_capabilities()
-- capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

-- local mason_lspconfig = require("mason-lspconfig")
-- mason_lspconfig.setup({
-- 	automatic_enable = true,
-- 	ensure_installed = servers,
-- })
-- mason_lspconfig.setup_handlers({
-- 	function(server_name)
-- 		require("lspconfig")[server_name].setup({
-- 			-- capabilities = capabilities,
-- 			on_attach = on_attach,
-- 			settings = servers[server_name],
-- 		})
-- 	end,
-- })

local border = "rounded"
vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
	border = border,
})
vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
	border = border,
})
vim.diagnostic.config({
	virtual_text = false,
	virtual_lines = false,
	float = {
		border = border,
		source = "if_many",
	},
})
require("lspconfig.ui.windows").default_options = {
	border = border,
}

return M
