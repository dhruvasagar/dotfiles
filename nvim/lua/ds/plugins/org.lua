return {
  {
    "nvim-orgmode/orgmode",
    event = "VeryLazy",
    ft = { "org" },
    opts = {
      org_agenda_span = "day",
      org_agenda_files = "~/src/dhruvasagar/org-files/**/*",
      org_default_notes_file = "~/src/dhruvasagar/org-files/refile.org",
      org_todo_keywords = {
        "TODO",
        "NEXT",
        "WAITING",
        "HOLD",
        "PHONE",
        "MEETING",
        "|",
        "CANCELLED",
        "DONE",
      },
      org_todo_keyword_faces = {
        TODO = ":foreground #df0000 :weight bold",
        NEXT = ":foreground #005fff :weight bold",
        DONE = ":foreground #005f00 :weight bold",
        WAITING = ":foreground #ff5f00 :weight bold",
        HOLD = ":foreground #5f005f :weight bold",
        CANCELLED = ":foreground #005f00 :weight bold",
        MEETING = ":foreground #005f00 :weight bold",
        PHONE = ":foreground #005f00 :weight bold",
      },
      org_todo_repeat_to_state = "NEXT",
      org_deadline_warning_days = 30,
      org_capture_templates = {
        t = {
          description = "Todo",
          template = "* TODO %?\nDEADLINE: %t\n",
        },
        n = {
          description = "Note",
          template = "* %? :NOTE:\n",
        },
        j = {
          description = "Journal",
          template = "\n*** %<%Y-%m-%d> %<%A>\n**** %U\n\n%?",
          target = "~/sync/org/journal.org",
        },
        m = {
          description = "Meeting",
          template = "* MEETING with %? :MEETING:",
        },
        p = {
          description = "Phone Call",
          template = "* PHONE %? :PHONE:",
        },
        h = {
          description = "Habit",
          template = "* NEXT %?\nSCHEDULED: %t\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:",
        },
      },
      mappings = {
        org = {
          org_capture_finalize = "<C-c><C-c>",
          org_open_at_point = "<Return>",
        },
      },
    },
  },
  {
    "chipsenkbeil/org-roam.nvim",
    enabled = true,
    dependencies = {
      "nvim-orgmode/orgmode",
    },
    config = function()
      require("org-roam").setup({
        directory = "~/src/dhruvasagar/org-files/roam/",
      })
    end,
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
