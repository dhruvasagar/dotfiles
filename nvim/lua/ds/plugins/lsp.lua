return {
  {
    "E-ricus/lsp_codelens_extensions.nvim",
    dependencies = { "nvim-lua/plenary.nvim", "mfussenegger/nvim-dap" },
    config = function()
      require("codelens_extensions").setup()
    end,
  },
  "williamboman/mason.nvim",
  {
    "williamboman/mason-lspconfig.nvim",
    dependencies = { "williamboman/mason.nvim", "neovim/nvim-lspconfig" },
  },
  {
    "j-hui/fidget.nvim",
    opts = {
      progress = {
        display = {
          progress_icon = { "moon" },
        },
      },
    },
  },
  {
    "jay-babu/mason-null-ls.nvim",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "williamboman/mason.nvim",
      "nvimtools/none-ls.nvim",
    },
    opt = {
      ensure_installed = {
        "prettierd",
        "stylua",
        "goimports",
        "terraform_fmt",
        "black",
        "shfmt",
        "terraform_validate",
        "google-java-format",
        "checkstyle",
      },
    },
  },
  {
    "nvimtools/none-ls.nvim",
    config = function()
      local nls = require("null-ls")
      local fmt = nls.builtins.formatting
      local dgn = nls.builtins.diagnostics
      local augroup = vim.api.nvim_create_augroup("LspFormatting", {})
      nls.setup({
        sources = {
          -- # FORMATTING #
          fmt.prettierd,
          fmt.stylua,
          fmt.goimports,
          fmt.terraform_fmt,
          fmt.black,
          -- fmt.google_java_format.with({ extra_args = { "--aosp" } }),
          -- fmt.uncrustify.with({
          --     extra_args = { "-c", "~/dotfiles/config/uncrustify.cfg" },
          -- }),
          fmt.shfmt.with({
            extra_args = { "-i", 4, "-ci", "-sr" },
          }),
          -- # DIAGNOSTICS #
          dgn.terraform_validate,
          dgn.checkstyle.with({
            extra_args = {
              "-c",
              vim.fn.expand("~/dotfiles/config/google_checks.xml"),
            },
          }),
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
  },
  {
    "iguanacucumber/magazine.nvim",
    name = "nvim-cmp",
    enabled = true,
    dependencies = {
      "hrsh7th/cmp-omni",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-buffer",
      "petertriho/cmp-git",
      "dmitmel/cmp-digraphs",
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-nvim-lsp-signature-help",
      "Saecki/crates.nvim",
      "David-Kunz/cmp-npm",
      "onsails/lspkind.nvim",
      "micangl/cmp-vimtex",
      "rcarriga/cmp-dap",
      "garymjr/nvim-snippets",
    },
    config = function()
      local cmp = require("cmp")
      local cmp_select_opts = { behavior = cmp.SelectBehavior.Select }
      vim.opt.completeopt = "menu,menuone,noselect"
      cmp.setup({
        snippet = {
          expand = function(args)
            vim.snippet.expand(args.body)
          end,
        },
        sources = {
          { name = "snippets" },
          { name = "orgmode" },
          { name = "codeium" },
          { name = "cody" },
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
        formatting = {
          format = require("lspkind").cmp_format({
            mode = "symbol",
            maxwidth = 50,
            ellipsis_char = "...",
            symbol_map = { Codeium = "" },
          }),
        },
      })
      -- Set configuration for specific filetype.
      cmp.setup.filetype("gitcommit", {
        sources = cmp.config.sources({
          { name = "git" }, -- You can specify the `cmp_git` source if you were installed it.
        }),
      })
      cmp.setup.filetype("tex", {
        sources = {
          { name = "vimtex" },
        },
      })
      cmp.setup.filetype({ "dap-repl", "dapui_watches", "dapui_hover" }, {
        sources = {
          { name = "dap" },
        },
      })
    end,
  },
  {
    "saghen/blink.cmp",
    enabled = false,
    event = "BufReadPre",
    version = "v0.*", -- REQUIRED release tag to download pre-built binaries
    dependencies = "rafamadriz/friendly-snippets",

    ---@module "blink.cmp"
    ---@type blink.cmp.Config
    opts = {
      -- 'default' for mappings similar to built-in completion
      -- 'super-tab' for mappings similar to vscode (tab to accept, arrow keys to navigate)
      -- 'enter' for mappings similar to 'super-tab' but with 'enter' to accept
      -- see the "default configuration" section below for full documentation on how to define
      -- your own keymap.
      keymap = { preset = "enter" },

      highlight = {
        -- sets the fallback highlight groups to nvim-cmp's highlight groups
        -- useful for when your theme doesn't support blink.cmp
        -- will be removed in a future release, assuming themes add support
        use_nvim_cmp_as_default = true,
      },
      -- set to 'mono' for 'Nerd Font Mono' or 'normal' for 'Nerd Font'
      -- adjusts spacing to ensure icons are aligned
      nerd_font_variant = "normal",

      -- experimental auto-brackets support
      -- accept = { auto_brackets = { enabled = true } }

      -- experimental signature help support
      -- trigger = { signature_help = { enabled = true } }
    },
  },
  {
    "rmagatti/goto-preview",
    config = function()
      require("goto-preview").setup({
        default_mappings = true,
      })
    end,
  },
  {
    "onsails/lspkind.nvim",
    enabled = true,
    config = function()
      require("lspkind").init({
        -- DEPRECATED (use mode instead): enables text annotations
        --
        -- default: true
        -- with_text = true,

        -- defines how annotations are shown
        -- default: symbol
        -- options: 'text', 'text_symbol', 'symbol_text', 'symbol'
        mode = "symbol_text",

        -- default symbol map
        -- can be either 'default' (requires nerd-fonts font) or
        -- 'codicons' for codicon preset (requires vscode-codicons font)
        --
        -- default: 'default'
        preset = "codicons",

        -- override preset symbols
        --
        -- default: {}
        symbol_map = {
          Text = "󰉿",
          Method = "󰆧",
          Function = "󰊕",
          Constructor = "",
          Field = "󰜢",
          Variable = "󰀫",
          Class = "󰠱",
          Interface = "",
          Module = "",
          Property = "󰜢",
          Unit = "󰑭",
          Value = "󰎠",
          Enum = "",
          Keyword = "󰌋",
          Snippet = "",
          Color = "󰏘",
          File = "󰈙",
          Reference = "󰈇",
          Folder = "󰉋",
          EnumMember = "",
          Constant = "󰏿",
          Struct = "󰙅",
          Event = "",
          Operator = "󰆕",
          TypeParameter = "",
        },
      })
    end,
  },
  { "mfussenegger/nvim-jdtls", enabled = false },
  { "nvim-java/nvim-java",     enabled = true },
}
