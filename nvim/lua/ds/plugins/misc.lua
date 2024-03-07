return {
  "andymass/vim-matchup",
  "wellle/targets.vim",
  "tommcdo/vim-exchange",
  "AndrewRadev/switch.vim",
  "AndrewRadev/sideways.vim",
  {
    "Wansmer/treesj",
    dependencies = { "nvim-treesitter" },
    config = true,
  },
  {
    "dgagn/diagflow.nvim",
    config = true,
  },
  "simnalamburt/vim-mundo",
  {
    "machakann/vim-sandwich",
    enabled = false,
  },
  {
    "kylechui/nvim-surround",
    config = function()
      require("nvim-surround").setup({
        surrounds = {
          ["v"] = {
            add = function()
              local ts_utils = require("nvim-treesitter.ts_utils")
              local cur = ts_utils.get_node_at_cursor(0, true)
              local language = vim.bo.filetype
              local is_jsy = (
                language == "javascript"
                or language == "javascriptreact"
                or language == "typescript"
                or language == "typescriptreact"
              )

              if is_jsy then
                local cur_type = cur:type()
                local interpolation_surround = { { "${" }, { "}" } }
                if cur and (cur_type == "string" or cur_type == "string_fragment") then
                  vim.cmd.normal("csq`")
                  return interpolation_surround
                elseif cur and cur_type == "template_string" then
                  return interpolation_surround
                else
                  return { { "`${" }, { "}`" } }
                end
              end
            end,
          },
        },
      })
    end,
  },
  "pangloss/vim-javascript",
  "MaxMEllon/vim-jsx-pretty",
  { "guns/vim-sexp",           lazy = true },
  { "guns/vim-clojure-static", lazy = true },
  "kana/vim-vspec",
  "avdgaag/vim-phoenix",
  "digitaltoad/vim-jade",
  "elixir-lang/vim-elixir",
  "slim-template/vim-slim",
  {
    "kristijanhusak/vim-dadbod-ui",
    dependencies = { "tpope/vim-dadbod" },
  },
  {
    "luckasRanarison/nvim-devdocs",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
      "nvim-treesitter/nvim-treesitter",
    },
    config = true,
  },
  "leafgarland/typescript-vim",
  {
    "pmizio/typescript-tools.nvim",
    dependencies = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    config = true,
  },
  "purescript-contrib/purescript-vim",
  "SirVer/ultisnips",
  "honza/vim-snippets",
  "benmills/vimux",
  {
    "nvim-neotest/neotest",
    dependencies = {
      "vim-test/vim-test",
      "nvim-neotest/neotest-go",
      "nvim-neotest/neotest-jest",
      "thenbe/neotest-playwright",
      "olimorris/neotest-rspec",
      "rouge8/neotest-rust",
      "jfpedroza/neotest-elixir",
      "mrcjkb/neotest-haskell",
      "lawrence-laz/neotest-zig",
    },
    config = function()
      require("neotest").setup({
        adapters = {
          require("neotest-go"),
          require("neotest-jest"),
          require("neotest-playwright"),
          require("neotest-rspec"),
          require("neotest-rust"),
          require("neotest-elixir"),
          require("neotest-haskell"),
          require("neotest-zig"),
        },
      })
    end,
  },
  "diepm/vim-rest-console",
  "powerman/vim-plugin-AnsiEsc",
  { "mattn/gist-vim",   dependencies = { "mattn/webapi-vim" } },
  "editorconfig/editorconfig-vim",
  "godlygeek/tabular",
  "vim-scripts/SyntaxRange",
  {
    "ellisonleao/glow.nvim",
    config = true,
  },
  "SidOfc/mkdx",
  { "kkoomen/vim-doge", build = ":call doge#install()" },
  "junegunn/goyo.vim",
  "junegunn/vim-emoji",
  "guns/xterm-color-table.vim",
  "ledger/vim-ledger",
  {
    "akinsho/toggleterm.nvim",
    config = function()
      require("toggleterm").setup({
        direction = "float",
        open_mapping = [[<c-\>]],
      })
    end,
  },
  {
    "lukas-reineke/indent-blankline.nvim",
    config = function()
      require("ibl").setup({
        exclude = {
          buftypes = { "terminal" },
        },
        scope = {
          enabled = true,
          show_end = false,
          highlight = "Title",
        },
        indent = {
          char = "│",
          tab_char = "│",
        },
      })
    end,
  },
  "dstein64/vim-startuptime",
  { "brenoprata10/nvim-highlight-colors", config = true },
  "jbyuki/venn.nvim",
  {
    "mattn/libcallex-vim",
    build = "make -C autoload",
  },
  {
    "bytesnake/vim-graphical-preview",
    build = "cargo build --release",
  },
  {
    "m4xshen/hardtime.nvim",
    dependencies = { "MunifTanjim/nui.nvim", "nvim-lua/plenary.nvim" },
  },
  {
    "stevearc/oil.nvim",
    config = function()
      require("oil").setup()
      vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })
    end,
  },
  {
    "phelipetls/vim-hugo",
  },
  "lervag/vimtex",
}
