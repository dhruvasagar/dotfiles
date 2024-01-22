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
        telescope = {
          style = "classic",
        },
        override = {
          DiffAdd = { fg = "#e4e4e4", bg = "#005f00" },
          DiffText = { fg = "#ffffff", bg = "#ff0000" },
          DiffChange = { fg = "#ffffff", bg = "#870087" },
          DiffDelete = { fg = "#000000", bg = "#5f0000" },
          ["@markup.strong"] = {
            fg = palette.red.base,
          },
          ["@markup.raw"] = {
            fg = palette.blue0,
          },
          ["@markup.italic"] = {
            bold = true,
            italic = true,
          },
          ["@markup.heading.1"] = {
            fg = palette.yellow.dim,
            bold = true,
          },
          ["@markup.heading.2"] = {
            fg = palette.green.dim,
            bold = true,
          },
          ["@markup.heading.3"] = {
            fg = palette.magenta.dim,
            bold = true,
          },
        },
      })
      vim.cmd("colorscheme nordic")
    end,
  },
  "pappasam/papercolor-theme-slim",
}
