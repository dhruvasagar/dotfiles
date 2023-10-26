local install_path = vim.fn.stdpath("data") .. "/site/pack/packer/start/packer.nvim"
local is_bootstrap = false
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
  is_bootstrap = true
  vim.fn.system({ "git", "clone", "--depth", "1", "https://github.com/wbthomason/packer.nvim", install_path })
end
vim.cmd([[packadd packer.nvim]])

return require("packer").startup(function(use)
  use("wbthomason/packer.nvim")

  use("tpope/vim-rsi")
  use("tpope/vim-rake")
  use("tpope/vim-tbone")
  use("tpope/vim-apathy")
  use("tpope/vim-repeat")
  use("tpope/vim-abolish")
  use("tpope/vim-endwise")
  use("tpope/vim-rhubarb")
  use({ "tpope/vim-dispatch", opt = true, cmd = { "Dispatch", "Make", "Focus", "Start" } })
  use("tpope/vim-fugitive")
  use("tpope/vim-sensible")
  use({ "tpope/vim-fireplace", opt = true })
  use({ "tpope/vim-leiningen", opt = true })
  use("tpope/vim-obsession")
  use("tpope/vim-commentary")
  use("tpope/vim-scriptease")
  use("tpope/vim-unimpaired")
  use("tpope/vim-speeddating")
  use("tpope/vim-characterize")
  use({ "tpope/vim-sexp-mappings-for-regular-people", opt = true })
  use("tpope/vim-rails")
  use("tpope/vim-eunuch")
  use("tpope/vim-bundler")
  use("tpope/vim-cucumber")

  use("andymass/vim-matchup")
  use("wellle/targets.vim")
  use("tommcdo/vim-exchange")
  use("AndrewRadev/switch.vim")
  use("AndrewRadev/sideways.vim")
  use({
    "Wansmer/treesj",
    requires = { "nvim-treesitter" },
    config = function()
      require("treesj").setup()
    end,
  })
  use({
    "dgagn/diagflow.nvim",
    config = function()
      require("diagflow").setup()
    end,
  })
  use("simnalamburt/vim-mundo")
  use("machakann/vim-sandwich")
  use("pangloss/vim-javascript")
  use("MaxMEllon/vim-jsx-pretty")
  use({ "guns/vim-sexp", opt = true })
  use({ "guns/vim-clojure-static", opt = true })
  use("kana/vim-vspec")
  use("avdgaag/vim-phoenix")
  use("digitaltoad/vim-jade")
  use("elixir-lang/vim-elixir")
  use("slim-template/vim-slim")
  use({
    "kristijanhusak/vim-dadbod-ui",
    requires = { "tpope/vim-dadbod" },
  })
  use({
    "luckasRanarison/nvim-devdocs",
    requires = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      require("nvim-devdocs").setup()
    end,
  })

  -- DAP
  use("mfussenegger/nvim-dap")
  use({
    "rcarriga/nvim-dap-ui",
    requires = { "mfussenegger/nvim-dap" },
    config = function()
      require("dapui").setup()
    end,
  })
  use({
    "leoluz/nvim-dap-go",
    requires = { "mfussenegger/nvim-dap" },
    config = function()
      require("dap-go").setup()
    end,
  })
  use({
    "jbyuki/one-small-step-for-vimkind",
    requires = { "mfussenegger/nvim-dap" },
  })
  use({
    "theHamsta/nvim-dap-virtual-text",
    requires = {
      "mfussenegger/nvim-dap",
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      require("nvim-dap-virtual-text").setup()
    end,
  })

  use("leafgarland/typescript-vim")
  use({
    "pmizio/typescript-tools.nvim",
    requires = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    config = function()
      require("typescript-tools").setup({})
    end,
  })
  use("purescript-contrib/purescript-vim")
  use("SirVer/ultisnips")
  use("honza/vim-snippets")
  use("benmills/vimux")
  use("vim-test/vim-test")
  use("diepm/vim-rest-console")
  use("powerman/vim-plugin-AnsiEsc")
  use({ "mattn/gist-vim", requires = { { "mattn/webapi-vim" } } })
  use("editorconfig/editorconfig-vim")
  use("godlygeek/tabular")
  use("vim-scripts/SyntaxRange")
  use({
    "nvim-telescope/telescope.nvim",
    requires = { "nvim-lua/plenary.nvim" },
    config = function()
      require("telescope").setup({
        defaults = {
          path_display = { "smart" },
          dynamic_preview_title = true,
        },
      })
    end,
  })
  use({
    "nvim-telescope/telescope-fzf-native.nvim",
    requires = { "nvim-telescope/telescope.nvim" },
    run = "make",
    cond = vim.fn.executable("make") == 1,
    config = function()
      require("telescope").load_extension("fzf")
    end,
  })
  use({
    "nvim-telescope/telescope-github.nvim",
    requires = { "nvim-telescope/telescope.nvim" },
    config = function()
      require("telescope").load_extension("gh")
    end,
  })
  use({
    "nvim-treesitter/nvim-treesitter",
    run = function()
      pcall(require("nvim-treesitter.install").update({ with_sync = true }))
    end,
    config = function()
      require("nvim-treesitter.configs").setup({
        ensure_installed = {
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
          "comment",
          "html",
          "ruby",
          "clojure",
          "bash"
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
        playground = {
          enable = true,
          keybindings = {
            toggle_query_editor = "o",
            toggle_hl_groups = "i",
            toggle_injected_languages = "t",
            toggle_anonymous_nodes = "a",
            toggle_language_display = "I",
            focus_language = "f",
            unfocus_language = "F",
            update = "R",
            goto_node = "<cr>",
            show_help = "?",
          },
        },
      })
    end,
  })
  use({
    "nvim-treesitter/nvim-treesitter-context",
    requires = { "nvim-treesitter/nvim-treesitter" },
    config = function()
      require("treesitter-context").setup()
    end,
  })
  use("nvim-treesitter/playground")
  use({ "nvim-treesitter/nvim-treesitter-textobjects", after = "nvim-treesitter" })
  -- use({
  -- 	"numToStr/Comment.nvim",
  -- 	config = function()
  -- 		require("Comment").setup()
  -- 	end,
  -- })
  use({
    "ellisonleao/glow.nvim",
    config = function()
      require("glow").setup()
    end,
  })
  use("SidOfc/mkdx")
  use({ "kkoomen/vim-doge", run = ":call doge#install()" })
  use("junegunn/goyo.vim")
  use("junegunn/vim-emoji")
  use("guns/xterm-color-table.vim")
  use("ledger/vim-ledger")
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
      require("ibl").setup({
        exclude = {
          buftypes = { "terminal" }
        },
        scope = {
          enabled = true,
          show_exact_scope = true
        },
        indent = {
          char = '│',
        }
      })
    end,
  })
  use("dstein64/vim-startuptime")
  use({
    "RRethy/vim-hexokinase",
    run = "make hexokinase",
  })
  use("jbyuki/venn.nvim")
  use({
    "folke/todo-comments.nvim",
    requires = "nvim-lua/plenary.nvim",
    config = function()
      require("todo-comments").setup({
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      })
    end,
  })
  use({
    "folke/trouble.nvim",
    requires = "nvim-tree/nvim-web-devicons",
    config = function()
      require("trouble").setup({
        -- your configuration comes here
        -- or leave it empty to use the default settings
        -- refer to the configuration section below
      })
    end,
  })
  use({
    "Bryley/neoai.nvim",
    requires = "MunifTanjim/nui.nvim",
    config = function()
      require("neoai").setup({
        ui = {
          output_popup_text = "NeoAI",
          input_popup_text = "Prompt",
          width = 30,               -- As percentage eg. 30%
          output_popup_height = 80, -- As percentage eg. 80%
        },
        model = "gpt-3.5-turbo",
        register_output = {
          ["g"] = function(output)
            return output
          end,
          ["c"] = require("neoai.utils").extract_code_snippets,
        },
        inject = {
          cutoff_width = 75,
        },
        prompts = {
          context_prompt = function(context)
            return "Hi ChatGPT, I'd like to provide some context for future "
                .. "messages. Here is the code/text that I want to refer "
                .. "to in our upcoming conversations:\n\n"
                .. context
          end,
        },
        open_api_key_env = "OPENAI_API_KEY",
        shortcuts = {
          {
            key = "<leader>as",
            use_context = true,
            prompt = [[
                Please rewrite the text to make it more readable, clear,
                concise, and fix any grammatical, punctuation, or spelling
                errors
            ]],
            modes = { "v" },
            strip_function = nil,
          },
          {
            key = "<leader>ag",
            use_context = false,
            prompt = function()
              return [[
                    Using the following git diff generate a consise and
                    clear git commit message, with a short title summary
                    that is 75 characters or less:
                ]] .. vim.fn.system("git diff --cached")
            end,
            modes = { "n" },
            strip_function = nil,
          },
        },
      })
    end,
  })
  -- Plug 'mattn/libcallex-vim', { 'do': 'make -C autoload' }
  -- Plug 'bytesnake/vim-graphical-preview', { 'do': 'cargo build --release' }

  use({
    "mattn/libcallex-vim",
    run = "make -C autoload",
  })
  use({
    "bytesnake/vim-graphical-preview",
    run = "cargo build --release",
  })

  -- LSP Plugins
  use({
    "E-ricus/lsp_codelens_extensions.nvim",
    requires = {
      { "nvim-lua/plenary.nvim" },
      { "mfussenegger/nvim-dap" },
    },
    config = function()
      require("codelens_extensions").setup()
    end,
  })
  use({
    "folke/neodev.nvim",
    config = function()
      require("neodev").setup()
    end,
  })
  use({
    "folke/neoconf.nvim",
  })
  use({
    "williamboman/mason.nvim",
  })
  use({ "williamboman/mason-lspconfig.nvim", requires = { "neovim/nvim-lspconfig" } })
  use({
    "j-hui/fidget.nvim",
    tag = "legacy",
    config = function()
      require("fidget").setup({
        text = {
          spinner = "moon",
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
          fmt.terraform_fmt,
          fmt.shfmt.with({
            extra_args = { "-i", 4, "-ci", "-sr" },
          }),
          -- # DIAGNOSTICS #
          dgn.shellcheck,
          dgn.luacheck.with({
            extra_args = { "--globals", "vim", "--std", "luajit" },
          }),
          dgn.terraform_validate,
        },
        on_attach = function(client, bufnr)
          if client.supports_method("textDocument/formatting") then
            vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
            vim.api.nvim_create_autocmd("BufWritePre", {
              group = augroup,
              buffer = bufnr,
              callback = function()
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
          { name = "buffer",                 keyword_length = 3 },
          { name = "nvim_lsp",               keyword_length = 3 },
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
  })
  use({
    "rmagatti/goto-preview",
    config = function()
      require("goto-preview").setup({
        default_mappings = true,
      })
    end,
  })
  use({
    "m4xshen/hardtime.nvim",
    requires = { 'MunifTanjim/nui.nvim', "nvim-lua/plenary.nvim" }
  })

  -- Colorschemes
  use({
    "EdenEast/nightfox.nvim",
    config = function()
      require("nightfox").setup({
        groups = {
          all = {
            DiffAdd = { fg = "#e4e4e4", bg = "#005f00" },
            DiffText = { fg = "#ffffff", bg = "#ff0000" },
            DiffChange = { fg = "#ffffff", bg = "#870087" },
            DiffDelete = { fg = "#000000", bg = "#5f0000" },
          },
        },
      })
      -- vim.cmd("colorscheme nightfox")
      -- vim.cmd("colorscheme duskfox")
      -- vim.cmd("colorscheme carbonfox")
    end,
  })
  use({
    "Shatur/neovim-ayu",
    config = function()
      require("ayu").setup({
        overrides = {
          DiffAdd = { fg = "#e4e4e4", bg = "#005f00" },
          DiffText = { fg = "#ffffff", bg = "#ff0000" },
          DiffChange = { fg = "#ffffff", bg = "#870087" },
          DiffDelete = { fg = "#000000", bg = "#5f0000" },
        },
      })
      -- vim.cmd("colorscheme ayu")
    end,
  })
  use({
    "AlexvZyl/nordic.nvim",
    config = function()
      require("nordic").setup({
        bright_border = true,
      })
      vim.cmd("colorscheme nordic")
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
