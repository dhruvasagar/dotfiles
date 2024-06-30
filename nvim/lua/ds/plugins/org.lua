return {
  {
    "nvim-orgmode/orgmode",
    event = "VeryLazy",
    ft = { "org" },
    config = function()
      require("orgmode").setup({
        org_agenda_files = "~/Dropbox/Documents/dotoo-files/",
        org_default_notes_file = "~/Dropbox/Documents/dotoo-files/refile.org",
      })
    end,
  },
  "chipsenkbeil/org-roam.nvim",
  dependencies = {
    {
      "nvim-orgmode/orgmode",
    },
  },
  opts = {
    directory = "~/Dropbox/Documents/dotoo-files/notes/",
  },
}
