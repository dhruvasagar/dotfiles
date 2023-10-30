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
  "vim-test/vim-test",
  "diepm/vim-rest-console",
  "powerman/vim-plugin-AnsiEsc",
  { "mattn/gist-vim",   dependencies = { { "mattn/webapi-vim" } } },
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
    "Bryley/neoai.nvim",
    dependencies = "MunifTanjim/nui.nvim",
    config = function()
      require("neoai").setup({
        ui = {
          output_popup_text = "NeoAI",
          input_popup_text = "Prompt",
          width = 30,          -- As percentage eg. 30%
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
  },
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
}
