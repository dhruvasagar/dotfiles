return {
  {
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
  },
  {
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
  },
  {
    "AlexvZyl/nordic.nvim",
    config = function()
      local palette = require("nordic.colors")
      require("nordic").setup({
        bright_border = true,
        cursorline = {
          bold = true,
          theme = "light",
        },
        on_highlight = function(highlights, palette)
          highlights.DiffAdd = { fg = "#e4e4e4", bg = "#005f00" }
          highlights.DiffText = { fg = "#ffffff", bg = "#ff0000" }
          highlights.DiffChange = { fg = "#ffffff", bg = "#870087" }
          highlights.DiffDelete = { fg = "#000000", bg = "#5f0000" }
          highlights.PmenuSel = { bg = "#434C5E" }
          highlights["@markup.strong"] = {
            fg = palette.red.base,
          }
          highlights["@markup.raw"] = {
            fg = palette.blue0,
          }
          highlights["@markup.italic"] = {
            bold = true,
            italic = true,
          }
          highlights["@markup.heading.1"] = {
            fg = palette.yellow.dim,
            bold = true,
          }
          highlights["@markup.heading.2"] = {
            fg = palette.green.dim,
            bold = true,
          }
          highlights["@markup.heading.3"] = {
            fg = palette.magenta.dim,
            bold = true,
          }
        end,
      })
      -- vim.cmd("colorscheme nordic")
    end,
  },
  {
    "ab-dx/ares.nvim",
    name = "ares",
    priority = 1000,
    dependencies = { "rktjmp/lush.nvim" },
    config = function()
      -- vim.cmd("colorscheme ares")
    end,
  },
  {
    "2giosangmitom/nightfall.nvim",
    lazy = false,
    priority = 1000,
    opts = {}, -- Add custom configuration here
    config = function(_, opts)
      -- vim.cmd("colorscheme nightfall") -- Choose from: nightfall, deeper-night, maron, nord
    end,
  },
  {
    "killitar/obscure.nvim",
    lazy = false,
    priority = 1000,
    opts = {},
    config = function(_, opts)
      -- vim.cmd("colorscheme obscure") -- Choose from: nightfall, deeper-night, maron, nord
    end,
  },
  {
    "neko-night/nvim",
    lazy = false,
    priority = 1000,
    opts = {},
    config = function(_, opts)
      vim.cmd([[colorscheme nekonight-moon]])
    end,
  },
}
