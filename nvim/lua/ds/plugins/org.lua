return {
  {
    "nvim-orgmode/orgmode",
    event = "VeryLazy",
    ft = { "org" },
    opts = {
      org_agenda_files = "~/Dropbox/Documents/org-files/**/*",
      org_default_notes_file = "~/Dropbox/Documents/org-files/refile.org",
      mappings = {
        org = {
          org_toggle_checkbox = "cic",
        },
      },
    },
  },
  {
    "chipsenkbeil/org-roam.nvim",
    dependencies = {
      {
        "nvim-orgmode/orgmode",
      },
    },
    opts = {
      directory = "~/Dropbox/Documents/org-files/notes/",
    },
  },
  {
    "nvim-orgmode/telescope-orgmode.nvim",
    event = "VeryLazy",
    dependencies = {
      "nvim-orgmode/orgmode",
      "nvim-telescope/telescope.nvim",
    },
    config = function()
      require("telescope").load_extension("orgmode")

      vim.keymap.set("n", "<leader>r", require("telescope").extensions.orgmode.refile_heading)
      vim.keymap.set("n", "<leader>fh", require("telescope").extensions.orgmode.search_headings)
      vim.keymap.set("n", "<leader>li", require("telescope").extensions.orgmode.insert_link)
    end,
  },
  {
    "akinsho/org-bullets.nvim",
    opts = {
      concealcursor = true,
    },
  },
}
