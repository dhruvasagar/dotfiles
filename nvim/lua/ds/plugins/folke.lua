return {
  {
    "folke/todo-comments.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = true,
  },
  {
    "folke/trouble.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    cmd = "Trouble",
    opts = {
      auto_preview = false,
    },
  },
  { "folke/neoconf.nvim", cmd = "Neoconf", config = true },
  -- {
  --   "folke/neodev.nvim",
  --   lazy = true,
  --   config = true,
  -- },
  {
    "folke/lazydev.nvim",
    ft = "lua",
    opts = {},
  },
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {},
    keys = {
      {
        "<Leader>?",
        function()
          require("which-key").show({ global = false })
        end,
        desc = "Buffer local Keymaps (which-key)",
      },
    },
  },
}
