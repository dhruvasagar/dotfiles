local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
local is_bootstrap = false
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  is_bootstrap = true
  vim.fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
  vim.cmd([[packadd packer.nvim]])
end

require("packer").startup(function(use)
  use("wbthomason/packer.nvim")

  use("tpope/vim-rsi")
  use("tpope/vim-rake")
  use("tpope/vim-tbone")
  use("tpope/vim-apathy")
  use("tpope/vim-repeat")
  use("tpope/vim-abolish")
  use("tpope/vim-endwise")
  use("tpope/vim-rhubarb")
  use({
    "tpope/vim-dispatch",
    opt = true,
    cmd = { "Dispatch", "Make", "Focus", "Start" },
  })
  use("tpope/vim-fugitive")
  use("tpope/vim-sensible")
  use({ "tpope/vim-fireplace", opt = true })
  use({ "tpope/vim-leiningen", opt = true })
  use("tpope/vim-obsession")
  -- use("tpope/vim-commentary")
  use("tpope/vim-scriptease")

  use("andymass/vim-matchup")
  use("tpope/vim-unimpaired")
  use("tpope/vim-speeddating")
  use("tpope/vim-characterize")
  use({ "tpope/vim-sexp-mappings-for-regular-people", opt = true })

  use("tpope/vim-rails")
  use("tpope/vim-eunuch")
  use("tpope/vim-bundler")
  use("tpope/vim-cucumber")

  use("wellle/targets.vim")
  use("tommcdo/vim-exchange")
  use("AndrewRadev/switch.vim")
  use("AndrewRadev/sideways.vim")
  use("AndrewRadev/splitjoin.vim")
  use("simnalamburt/vim-mundo")
  use("machakann/vim-sandwich")

  use("pangloss/vim-javascript")
  use("MaxMEllon/vim-jsx-pretty")
  use("burnettk/vim-angular")

  use({ "guns/vim-sexp", opt = true })
  use({ "guns/vim-clojure-static", opt = true })

  use("neovimhaskell/haskell-vim")

  use("kana/vim-vspec")
  use("avdgaag/vim-phoenix")
  use("digitaltoad/vim-jade")
  use("elixir-lang/vim-elixir")
  use("slim-template/vim-slim")
  -- use { 'fatih/vim-go', run = ':GoInstallBinaries' }

  -- use 'simrat39/rust-tools.nvim'

  use({
    "rcarriga/nvim-dap-ui",
    requires = { "mfussenegger/nvim-dap" },
  })
  use("leafgarland/typescript-vim")
  use("purescript-contrib/purescript-vim")
  use("SirVer/ultisnips")
  use("honza/vim-snippets")
  use("benmills/vimux")
  use("janko-m/vim-test")
  -- use 'dense-analysis/ale'
  use("diepm/vim-rest-console")
  use("powerman/vim-plugin-AnsiEsc")
  use({
    "mattn/gist-vim",
    requires = { { "mattn/webapi-vim" } },
  })
  use("editorconfig/editorconfig-vim")
  use("godlygeek/tabular")
  use("vim-scripts/SyntaxRange")
  use({
    "nvim-telescope/telescope.nvim",
    requires = { { "nvim-lua/plenary.nvim" } },
  })
  use({
    "nvim-telescope/telescope-fzf-native.nvim",
    run = "make",
    cond = vim.fn.executable("make") == 1,
    config = function()
      pcall(require("telescope").load_extension, "fzf")
    end,
  })
  use({
    "nvim-treesitter/nvim-treesitter",
    run = function()
      pcall(require("nvim-treesitter.install").update({ with_sync = true }))
    end,
    config = function()
      local parser_configs = require("nvim-treesitter.parsers").get_parser_configs()
      parser_configs.norg = {
        install_info = {
          url = "https://github.com/nvim-neorg/tree-sitter-norg",
          files = { "src/parser.c", "src/scanner.cc" },
          branch = "main",
        },
      }
      require("nvim-treesitter.configs").setup({
        ensure_installed = {
          "norg",
          "haskell",
          "cpp",
          "c",
          "javascript",
          "rust",
          "typescript",
          "go",
          "help",
          "vim",
          "lua",
          "python",
        },
        highlight = { enable = true },
        indent = { enable = true, disable = { "python" } },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "<c-space>",
            node_incremental = "<c-space>",
            scope_incremental = "<c-s>",
            node_decremental = "<c-backspace>",
          },
        },
        textobjects = {
          select = {
            enable = true,
            lookahead = true, -- Automatically jump forward to textobj, similar to targets.vim
            keymaps = {
              -- You can use the capture groups defined in textobjects.scm
              ["aa"] = "@parameter.outer",
              ["ia"] = "@parameter.inner",
              ["af"] = "@function.outer",
              ["if"] = "@function.inner",
              ["ac"] = "@class.outer",
              ["ic"] = "@class.inner",
            },
          },
          move = {
            enable = true,
            set_jumps = true, -- whether to set jumps in the jumplist
            goto_next_start = {
              ["]m"] = "@function.outer",
              ["]]"] = "@class.outer",
            },
            goto_next_end = {
              ["]M"] = "@function.outer",
              ["]["] = "@class.outer",
            },
            goto_previous_start = {
              ["[m"] = "@function.outer",
              ["[["] = "@class.outer",
            },
            goto_previous_end = {
              ["[M"] = "@function.outer",
              ["[]"] = "@class.outer",
            },
          },
          swap = {
            enable = true,
            swap_next = {
              ["<leader>a"] = "@parameter.inner",
            },
            swap_previous = {
              ["<leader>A"] = "@parameter.inner",
            },
          },
        },
      })
    end,
  })
  use({ -- Additional text objects via treesitter
    "nvim-treesitter/nvim-treesitter-textobjects",
    after = "nvim-treesitter",
  })

  use({
    "numToStr/Comment.nvim",
    config = function()
      require("Comment").setup()
    end,
  })

  -- LSP plugins
  use({
    "folke/neodev.nvim",
    config = function()
      require("neodev").setup()
    end,
  })

  use({
    "neovim/nvim-lspconfig",
    requires = {
      { "williamboman/mason.nvim" },
      { "williamboman/mason-lspconfig.nvim" },
      { "j-hui/fidget.nvim" },
    },
  })

  use({
    "glepnir/lspsaga.nvim",
    config = function()
      require("lspsaga").setup({
        lightbulb = {
          enable = false,
        },
        symbol_in_winbar = {
          enable = false,
          show_file = false,
          color_mode = true,
        },
      })
    end,
  })

  use({
    "jose-elias-alvarez/null-ls.nvim",
    config = function()
      local nls = require("null-ls")
      local fmt = nls.builtins.formatting
      local dgn = nls.builtins.diagnostics
      local augroup = vim.api.nvim_create_augroup("LspFormatting", {})
      nls.setup({
        sources = {
          -- # FORMATTING #
          fmt.trim_whitespace.with({
            filetypes = { "text", "sh", "zsh", "toml", "make", "conf", "tmux" },
          }),
          -- NOTE:
          -- 1. both needs to be enabled to so prettier can apply eslint fixes
          -- 2. prettierd should come first to prevent occassional race condition
          fmt.prettierd,
          fmt.eslint_d,
          fmt.rustfmt,
          fmt.fourmolu,
          fmt.stylua,
          fmt.goimports,
          fmt.shfmt.with({
            extra_args = { "-i", 4, "-ci", "-sr" },
          }),
          -- # DIAGNOSTICS #
          dgn.eslint_d,
          dgn.shellcheck,
          dgn.luacheck.with({
            extra_args = { "--globals", "vim", "--std", "luajit" },
          }),
        },
        on_attach = function(client, bufnr)
          if client.supports_method("textDocument/formatting") then
            vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
            vim.api.nvim_create_autocmd("BufWritePre", {
              group = augroup,
              buffer = bufnr,
              callback = function()
                -- on 0.8, you should use vim.lsp.buf.format({ bufnr = bufnr }) instead
                -- vim.lsp.buf.formatting_sync()
                vim.lsp.buf.format({ bufnr = bufnr })
              end,
            })
          end
        end,
      })
    end,
  })

  use({
    "hrsh7th/nvim-cmp",
    config = function()
      local cmp = require("cmp")
      local cmp_select_opts = { behavior = cmp.SelectBehavior.Select }
      vim.opt.completeopt = "menu,menuone,noselect"
      cmp.setup({
        snippet = {
          expand = function(args)
            vim.fn["UltiSnips#Anon"](args.body)
          end,
        },
        sources = {
          { name = "ultisnips" },
          { name = "path" },
          { name = "omni" },
          { name = "buffer", keyword_length = 3 },
          { name = "nvim_lsp", keyword_length = 3 },
          { name = "nvim_lsp_signature_help" },
        },
        mapping = {
          ["<CR>"] = cmp.mapping.confirm({ select = true }),

          ["<C-f>"] = cmp.mapping.scroll_docs(5),
          ["<C-u>"] = cmp.mapping.scroll_docs(-5),

          ["<C-e>"] = cmp.mapping.abort(),

          ["<C-p>"] = cmp.mapping.select_prev_item(cmp_select_opts),
          ["<C-n>"] = cmp.mapping.select_next_item(cmp_select_opts),

          -- when menu is visible, navigate to next item
          -- when line is empty, insert a tab character
          -- else, activate completion
          ["<Tab>"] = cmp.mapping(function(fallback)
            local col = vim.fn.col(".") - 1

            if cmp.visible() then
              cmp.select_next_item(cmp_select_opts)
            elseif col == 0 or vim.fn.getline("."):sub(col, col):match("%s") then
              fallback()
            else
              cmp.complete()
            end
          end, { "i", "s" }),

          -- when menu is visible, navigate to previous item on list
          -- else, revert to default behavior
          ["<S-Tab>"] = cmp.mapping(function(fallback)
            if cmp.visible() then
              cmp.select_prev_item(cmp_select_opts)
            else
              fallback()
            end
          end, { "i", "s" }),
        },
        window = {
          documentation = vim.tbl_deep_extend("force", cmp.config.window.bordered(), {
            max_height = 15,
            max_width = 60,
          }),
        },
      })
      -- Set configuration for specific filetype.
      cmp.setup.filetype("gitcommit", {
        sources = cmp.config.sources({
          { name = "git" }, -- You can specify the `cmp_git` source if you were installed it.
        }),
      })
    end,
    requires = {
      "hrsh7th/cmp-omni",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-buffer",
      "petertriho/cmp-git",
      "dmitmel/cmp-digraphs",
      "hrsh7th/cmp-nvim-lsp",
      "quangnguyen30192/cmp-nvim-ultisnips",
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "Saecki/crates.nvim",
      "David-Kunz/cmp-npm",
    },
  })

  use({
    "rmagatti/goto-preview",
    config = function()
      require("goto-preview").setup({
        default_mappings = true,
      })
    end,
  })

  use("ellisonleao/glow.nvim")

  use("SidOfc/mkdx")
  use({
    "kkoomen/vim-doge",
    run = function()
      vim.fn["doge#install"](0)
    end,
  })
  use("junegunn/goyo.vim")
  use("vim-scripts/DrawIt")
  use("junegunn/vim-emoji")
  use("norcalli/nvim-colorizer.lua")
  use("guns/xterm-color-table.vim")
  use({ "Rigellute/rigel" })
  use("ledger/vim-ledger")
  use("towolf/vim-helm")
  use("Lenovsky/nuake")
  use({
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("indent_blankline").setup({
        buftype_exclude = { "terminal" },
        space_char_blankline = " ",
        show_current_context = true,
        -- show_current_context_start = true,
      })
    end,
  })
  use({
    "pwntester/octo.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "kyazdani42/nvim-web-devicons",
    },
    config = function()
      require("octo").setup()
    end,
  })
  use("mustache/vim-mustache-handlebars")

  use("dstein64/vim-startuptime")

  use({
    "EdenEast/nightfox.nvim",
    config = function()
      vim.cmd("colorscheme nightfox")
      -- vim.cmd("colorscheme duskfox")
    end,
  })

  use("~/dotfiles/vim/pack/packup/start/vim-less")
  use("~/dotfiles/vim/pack/packup/start/vim-marp")
  use("~/dotfiles/vim/pack/packup/start/vim-zoom")
  use("~/dotfiles/vim/pack/packup/start/vim-dotoo")
  use("~/dotfiles/vim/pack/packup/start/vim-pairify")
  use("~/dotfiles/vim/pack/packup/start/vim-testify")
  use("~/dotfiles/vim/pack/packup/start/vim-open-url")
  use("~/dotfiles/vim/pack/packup/start/vim-table-mode")
  use("~/dotfiles/vim/pack/packup/start/vim-buffer-history")
  use({
    "~/dotfiles/vim/pack/packup/start/vim-railscasts-theme",
    config = function()
      -- vim.cmd("colorscheme railscasts")
    end,
  })
  use("~/dotfiles/vim/pack/packup/start/vim-github-review")
  use("~/dotfiles/vim/pack/packup/start/vim-comp")
  use("~/dotfiles/vim/pack/packup/opt/vim-prosession")

  if is_bootstrap then
    require("packer").sync()
  end
end)

-- Automatically source and re-compile packer whenever you save this init.lua
local packer_group = vim.api.nvim_create_augroup("Packer", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", {
  command = "source <afile> | silent! LspStop | silent! LspStart | PackerCompile",
  group = packer_group,
  pattern = vim.fn.expand("$MYVIMRC"),
})

-- LSP settings.
--  This function gets run when an LSP connects to a particular buffer.
local on_attach = function(_, bufnr)
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

  nmap("<leader>rn", vim.lsp.buf.rename, "[R]e[n]ame")
  nmap("<leader>ca", vim.lsp.buf.code_action, "[C]ode [A]ction")

  nmap("gd", vim.lsp.buf.definition, "[G]oto [D]efinition")
  nmap("gr", require("telescope.builtin").lsp_references, "[G]oto [R]eferences")
  nmap("gI", vim.lsp.buf.implementation, "[G]oto [I]mplementation")
  nmap("<leader>D", vim.lsp.buf.type_definition, "Type [D]efinition")
  nmap("<leader>ds", require("telescope.builtin").lsp_document_symbols, "[D]ocument [S]ymbols")
  nmap("<leader>ws", require("telescope.builtin").lsp_dynamic_workspace_symbols, "[W]orkspace [S]ymbols")

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

  nmap("gl", vim.diagnostic.open_float, "[O]pen [D]iagnostics")
  nmap("[d", vim.diagnostic.goto_prev, "[G]oto [P]revious Diagnostics")
  nmap("[d", vim.diagnostic.goto_prev, "[G]oto [N]ext Diagnostics")
end

local servers = {
  eslint = {},
  gopls = {},
  gradle_ls = {},
  grammarly = {},
  hls = {},
  jsonls = {},
  marksman = {},
  rust_analyzer = {},
  tsserver = {},
  vimls = {},
  yamlls = {},
  sumneko_lua = {
    Lua = {
      workspace = { checkThirdParty = false },
      telemetry = { enable = false },
    },
  },
}

require("mason").setup()

--
-- nvim-cmp supports additional completion capabilities, so broadcast that to servers
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

local mason_lspconfig = require("mason-lspconfig")
mason_lspconfig.setup({
  ensure_installed = vim.tbl_keys(servers),
})
mason_lspconfig.setup_handlers({
  function(server_name)
    require("lspconfig")[server_name].setup({
      capabilities = capabilities,
      on_attach = on_attach,
      settings = servers[server_name],
    })
  end,
})

require("fidget").setup()

local border = "rounded"
vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
  border = border,
})
vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
  border = border,
})
vim.diagnostic.config({
  float = { border = border },
})
require("lspconfig.ui.windows").default_options = {
  border = border,
}
