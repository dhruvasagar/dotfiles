local fn = vim.fn
local install_path = fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({ "git", "clone", "https://github.com/wbthomason/packer.nvim", install_path })
end

vim.cmd("packadd packer.nvim")

return require("packer").startup(function()
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
  use("tpope/vim-commentary")
  use("tpope/vim-scriptease")

  use({
    "folke/neodev.nvim",
    config = function()
      require("neodev").setup({})
    end,
    requires = { "neovim/nvim-lspconfig" }, -- {'hrsh7th/cmp-nvim-lsp'}},
  })
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
  use("vim-test/vim-test")
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
    "nvim-treesitter/nvim-treesitter",
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
        },
      })
    end,
    run = ":TSUpdate",
  })

  use({
    "VonHeikemen/lsp-zero.nvim",
    requires = {
      { "neovim/nvim-lspconfig" },
      { "williamboman/mason.nvim" },
      { "williamboman/mason-lspconfig.nvim" },
      { "jose-elias-alvarez/null-ls.nvim" },
    },
    config = function()
      local lsp = require("lsp-zero")
      lsp.preset("lsp-only")
      lsp.setup()
    end,
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
  -- use {
  --   'RRethy/vim-hexokinase',
  --   run = 'make hexokinase'
  -- }
  use("norcalli/nvim-colorizer.lua")
  use("guns/xterm-color-table.vim")
  use({ "Rigellute/rigel" })
  -- use("vimwiki/vimwiki")
  use("ledger/vim-ledger")
  -- use 'puremourning/vimspector'
  -- use 'Yggdroot/indentLine'
  use("towolf/vim-helm")
  use({
    "akinsho/toggleterm.nvim",
    config = function()
      require("toggleterm").setup({
        direction = "float",
        open_mapping = [[<c-\>]],
      })
    end,
  })
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
      "nvim-tree/nvim-web-devicons",
    },
    config = function()
      require("octo").setup()
    end,
  })
  use("mustache/vim-mustache-handlebars")

  use("dstein64/vim-startuptime")

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
      vim.cmd("colorscheme railscasts")
    end,
  })
  use("~/dotfiles/vim/pack/packup/start/vim-github-review")
  use("~/dotfiles/vim/pack/packup/start/vim-comp")
  use("~/dotfiles/vim/pack/packup/opt/vim-prosession")
end)
