return {
  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = true,
  },
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("trouble").setup({
        auto_preview = false,
      })
    end,
  },
  { "folke/neoconf.nvim", cmd = "Neoconf", config = true },
  {
    "folke/neodev.nvim",
    lazy = true,
    config = true,
  },
}
