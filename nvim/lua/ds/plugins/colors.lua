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
      require("nordic").setup({
        bright_border = true,
        cursorline = {
          bold = true,
          theme = "light",
        },
      })
      vim.cmd("colorscheme nordic")
    end,
  },
  "pappasam/papercolor-theme-slim",
}
