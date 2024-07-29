return {
  { dir = "~/dotfiles/vim/pack/packup/start/vim-less" },
  { dir = "~/dotfiles/vim/pack/packup/start/vim-marp" },
  { dir = "~/dotfiles/vim/pack/packup/start/vim-zoom" },
  { dir = "~/dotfiles/vim/pack/packup/start/vim-dotoo" },
  { dir = "~/dotfiles/vim/pack/packup/start/vim-pairify" },
  { dir = "~/dotfiles/vim/pack/packup/start/vim-testify" },
  { dir = "~/dotfiles/vim/pack/packup/start/vim-open-url" },
  { dir = "~/dotfiles/vim/pack/packup/start/vim-table-mode/" },
  { dir = "~/dotfiles/vim/pack/packup/start/vim-buffer-history" },
  {
    dir = "~/dotfiles/vim/pack/packup/start/vim-railscasts-theme",
    config = function()
      -- vim.cmd("colorscheme railscasts")
    end,
  },
  { dir = "~/dotfiles/vim/pack/packup/start/vim-github-review" },
  { dir = "~/dotfiles/vim/pack/packup/start/vim-comp" },
  {
    dir = "~/dotfiles/vim/pack/packup/opt/vim-prosession",
    dependencies = { "tpope/vim-obsession", "nvim-telescope/telescope.nvim" },
    config = function()
      require("telescope").load_extension("prosession")
    end,
  },
}
