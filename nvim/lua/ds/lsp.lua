-- LSP settings.
--  This function gets run when an LSP connects to a particular buffer.
local M = {}

local on_attach = function(client, bufnr)
  vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc", { buf = bufnr })

  -- NOTE: Remember that lua is a real programming language, and as such it is possible
  -- to define small helper and utility functions so you don't have to repeat yourself
  -- many times.
  --
  -- In this case, we create a function that lets us more easily define mappings specific
  -- for LSP related items. It sets the mode, buffer and description for us each time.
  local nmap = function(keys, func, desc)
    if desc then
      desc = "LSP: " .. desc
    end

    vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
  end

  nmap("gd", vim.lsp.buf.definition, "[G]oto [D]efinition")
  nmap("gr", vim.lsp.buf.references, "[G]oto [R]eferences")
  nmap("gI", vim.lsp.buf.implementation, "[G]oto [I]mplementation")
  nmap("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")
  nmap("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction")
  nmap("<leader>D", vim.lsp.buf.type_definition, "Type [D]efinition")
  nmap("<leader>ds", require("telescope.builtin").lsp_document_symbols, "[D]ocument [S]ymbols")
  nmap("<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")
  nmap("<leader>lr", vim.lsp.codelens.run, "[R]un [C]odelens")
  nmap("<Leader>ih", function()
    vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled())
  end, "[I]nlay [H]ints")

  -- See `:help K` for why this keymap
  nmap("K", vim.lsp.buf.hover, "Hover Documentation")
  nmap("<C-k>", vim.lsp.buf.signature_help, "Signature Documentation")

  -- Lesser used LSP functionality
  nmap("gD", vim.lsp.buf.declaration, "[G]oto [D]eclaration")
  nmap("<leader>wa", vim.lsp.buf.add_workspace_folder, "[W]orkspace [A]dd Folder")
  nmap("<leader>wr", vim.lsp.buf.remove_workspace_folder, "[W]orkspace [R]emove Folder")
  nmap("<leader>wl", function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, "[W]orkspace [L]ist Folders")

  -- Diagnostics
  nmap("gl", vim.diagnostic.open_float, "[O]pen [D]iagnostics")
  nmap("[d", vim.diagnostic.goto_prev, "[G]oto [P]revious Diagnostics")
  nmap("]d", vim.diagnostic.goto_next, "[G]oto [N]ext Diagnostics")

  -- vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI", "InsertLeave" }, {
  --   group = vim.api.nvim_create_augroup("LspCodelens", {}),
  --   callback = function()
  --     if client.server_capabilities.codeLensProvider and client.supports_method("textDocument/codeLens") then
  --       pcall(vim.lsp.codelens.refresh, { bufnr = 0 })
  --     end
  --   end,
  -- })

  if client.server_capabilities.inlayHintProvider then
    vim.lsp.inlay_hint.enable(true)
  end
end

M.on_attach = on_attach

require("java").setup({
  spring_boot_tools = {
    enabled = false,
  },
})

require("mason").setup()

local servers = {
  angularls = {},
  clojure_lsp = {},
  ts_ls = {},
  dockerls = {},
  eslint = {},
  gopls = {},
  gradle_ls = {},
  hls = {},
  texlab = {},
  jsonls = {},
  marksman = {},
  rust_analyzer = {
    check = "clippy",
  },
  vimls = {},
  terraformls = {},
  yamlls = {},
  pyright = {},
  lua_ls = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
      hint = { enable = true },
      diagnostics = {
        globals = { "vim", "require" },
      },
    },
  },
  jdtls = {
    settings = {
      java = {
        configuration = {
          runtimes = {
            {
              name = "Temurin Java 8",
              path = "/Users/dhruva/.asdf/installs/java/temurin-22.0.1+8",
              default = true,
            },
          },
        },
      },
    },
  },
}

--
-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
-- local capabilities = vim.lsp.protocol.make_client_capabilities()
-- capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

local mason_lspconfig = require("mason-lspconfig")
mason_lspconfig.setup({
  ensure_installed = vim.tbl_keys(servers),
})
mason_lspconfig.setup_handlers({
  function(server_name)
    require("lspconfig")[server_name].setup({
      -- capabilities = capabilities,
      on_attach = on_attach,
      settings = servers[server_name],
    })
  end,
})

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
