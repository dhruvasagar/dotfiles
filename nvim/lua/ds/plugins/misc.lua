return {
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
    opts = {
      placement = "inline",
    },
  },
  {
    "machakann/vim-sandwich",
    enabled = false,
  },
  {
    "kylechui/nvim-surround",
    opts = {
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
    },
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
    enabled = false,
  },
  "purescript-contrib/purescript-vim",
  {
    "garymjr/nvim-snippets",
    keys = {
      {
        "<Tab>",
        function()
          if vim.snippet.active({ direction = 1 }) then
            vim.schedule(function()
              vim.snippet.jump(1)
            end)
            return
          end
          return "<Tab>"
        end,
        expr = true,
        silent = true,
        mode = "i",
      },
      {
        "<Tab>",
        function()
          vim.schedule(function()
            vim.snippet.jump(1)
          end)
        end,
        expr = true,
        silent = true,
        mode = "s",
      },
      {
        "<S-Tab>",
        function()
          if vim.snippet.active({ direction = -1 }) then
            vim.schedule(function()
              vim.snippet.jump(-1)
            end)
            return
          end
          return "<S-Tab>"
        end,
        expr = true,
        silent = true,
        mode = { "i", "s" },
      },
    },
    opts = {
      friendly_snippets = true,
    },
  },
  { "rafamadriz/friendly-snippets" },
  "benmills/vimux",
  {
    "nvim-neotest/neotest",
    dependencies = {
      "nvim-neotest/nvim-nio",
      "nvim-lua/plenary.nvim",
      "antoinemadec/FixCursorHold.nvim",
      "nvim-treesitter/nvim-treesitter",
      "vim-test/vim-test",
      "fredrikaverpil/neotest-golang",
      "nvim-neotest/neotest-jest",
      "thenbe/neotest-playwright",
      "olimorris/neotest-rspec",
      "rouge8/neotest-rust",
      "jfpedroza/neotest-elixir",
      "lawrence-laz/neotest-zig",
      "rcasia/neotest-java",
    },
    config = function()
      require("neotest").setup({
        adapters = {
          require("neotest-golang")({
            go_test_args = {
              "-v",
              "-race",
              "-count=1",
              "-coverprofile=" .. vim.fn.getcwd() .. "/coverage.out",
            },
          }),
          require("neotest-jest"),
          require("neotest-playwright"),
          require("neotest-rspec"),
          require("neotest-rust"),
          require("neotest-elixir"),
          require("neotest-zig"),
          require("neotest-java"),
        },
      })
    end,
  },
  "diepm/vim-rest-console",
  "powerman/vim-plugin-AnsiEsc",
  { "mattn/gist-vim",              dependencies = { "mattn/webapi-vim" } },
  "editorconfig/editorconfig-vim",
  "godlygeek/tabular",
  "vim-scripts/SyntaxRange",
  {
    "ellisonleao/glow.nvim",
    config = true,
  },
  "SidOfc/mkdx",
  { "kkoomen/vim-doge",                   build = ":call doge#install()" },
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
    enabled = false,
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
    "stevearc/oil.nvim",
    config = function()
      require("oil").setup({
        view_options = {
          show_hidden = true,
        },
      })
      vim.keymap.set("n", "-", "<CMD>Oil<CR>", { desc = "Open parent directory" })
    end,
  },
  {
    "phelipetls/vim-hugo",
  },
  "lervag/vimtex",
  {
    "ThePrimeagen/harpoon",
    branch = "harpoon2",
    dependencies = { "nvim-lua/plenary.nvim" },
    enabled = false,
  },
  {
    "otavioschwanck/arrow.nvim",
    opts = {
      show_icons = true,
      leader_key = "<Leader>f", -- Recommended to be a single key
    },
  },
  {
    "ThePrimeagen/refactoring.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
    },
    opts = {},
  },
  "bullets-vim/bullets.vim",
  {
    "iamcco/markdown-preview.nvim",
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    ft = { "markdown" },
    build = function()
      vim.fn["mkdp#util#install"]()
    end,
  },
  "mityu/vim-applescript",
  "aklt/plantuml-syntax",
  {
    "vhyrro/luarocks.nvim",
    priority = 1001,
    opts = {
      rocks = { "magick" },
    },
  },
  {
    "3rd/image.nvim",
    enabled = false,
    config = function()
      require("image").setup({
        backend = "kitty",
        kitty_method = "normal",
        integrations = {
          markdown = {
            enabled = true,
            clear_in_insert_mode = false,
            download_remote_images = true,
            only_render_image_at_cursor = true,
            filetypes = { "markdown", "vimwiki" }, -- markdown extensions (ie. quarto) can go here
            resolve_image_path = function(document_path, image_path, fallback)
              return fallback(document_path, image_path)
            end,
          },
          html = {
            enabled = false,
          },
          css = {
            enabled = false,
          },
        },
        max_width = nil,
        max_height = nil,
        max_width_window_percentage = nil,
        max_height_window_percentage = 50,
        window_overlap_clear_enabled = false,                                           -- toggles images when windows are overlapped
        window_overlap_clear_ft_ignore = { "cmp_menu", "cmp_docs", "" },
        editor_only_render_when_focused = false,                                        -- auto show/hide images when the editor gains/looses focus
        tmux_show_only_in_active_window = false,                                        -- auto show/hide images in the correct Tmux window (needs visual-activity off)
        hijack_file_patterns = { "*.png", "*.jpg", "*.jpeg", "*.gif", "*.webp", "*.avif" }, -- render image files as images when opened
      })
    end,
  },
  {
    "NeogitOrg/neogit",
    enabled = false,
    dependencies = {
      "nvim-lua/plenary.nvim", -- required
      -- "sindrets/diffview.nvim", -- optional - Diff integration

      -- Only one of these is needed.
      "nvim-telescope/telescope.nvim", -- optional
    },
    opts = {},
    config = function()
      vim.cmd("hi NeogitDiffAddHighlight guifg=#000")
      vim.cmd("hi NeogitDiffDeletedHighlight guifg=#000")
    end,
  },
  {
    "andymass/vim-matchup",
    setup = function()
      vim.g.matchup_matchparen_offscreen = { method = "popup" }
    end,
  },
  {
    "wurli/visimatch.nvim",
    opts = {},
  },
}
