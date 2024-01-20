return {
  {
    "nvim-telescope/telescope.nvim",
    dependencies = { "nvim-lua/plenary.nvim" },
    config = function()
      require("telescope").setup({
        defaults = {
          path_dosplay = { "smart" },
          dynamic_preview_title = true,
        },
      })
    end,
  },
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    enabled = false,
    build = "make",
    dependencies = { "nvim-telescope/telescope.nvim" },
    cond = function()
      return vim.fn.executable("make") == 1
    end,
    config = function()
      require("telescope").load_extension("fzf")
    end,
  },
  {
    "nvim-telescope/telescope-github.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    config = function()
      require("telescope").load_extension("gh")
    end,
  },
}
