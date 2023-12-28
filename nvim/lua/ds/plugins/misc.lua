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
  "machakann/vim-sandwich",
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
  {
    "RRethy/vim-hexokinase",
    build = "make hexokinase",
  },
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
}
