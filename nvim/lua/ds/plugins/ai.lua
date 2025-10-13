return {
  {
    "Exafunction/codeium.nvim",
    enabled = false,
    dependencies = { "nvim-lua/plenary.nvim", "hrsh7th/nvim-cmp" },
    config = true,
    opts = {
      tools = {
        language_server = "/usr/local/bin/language_server_macos_arm",
      },
    },
  },
  {
    "GeorgesAlkhouri/nvim-aider",
    cmd = "Aider",
    -- Example key mappings for common actions:
    keys = {
      { "<leader>a/", "<cmd>Aider toggle<cr>",       desc = "Toggle Aider" },
      {
        "<leader>as",
        "<cmd>Aider send<cr>",
        desc = "Send to Aider",
        mode = { "n", "v" },
      },
      { "<leader>ac", "<cmd>Aider command<cr>",      desc = "Aider Commands" },
      { "<leader>ab", "<cmd>Aider buffer<cr>",       desc = "Send Buffer" },
      { "<leader>a+", "<cmd>Aider add<cr>",          desc = "Add File" },
      { "<leader>a-", "<cmd>Aider drop<cr>",         desc = "Drop File" },
      { "<leader>ar", "<cmd>Aider add readonly<cr>", desc = "Add Read-Only" },
      { "<leader>aR", "<cmd>Aider reset<cr>",        desc = "Reset Session" },
      -- Example nvim-tree.lua integration if needed
      { "<leader>a+", "<cmd>AiderTreeAddFile<cr>",   desc = "Add File from Tree to Aider",    ft = "NvimTree" },
      { "<leader>a-", "<cmd>AiderTreeDropFile<cr>",  desc = "Drop File from Tree from Aider", ft = "NvimTree" },
    },
    dependencies = {
      "folke/snacks.nvim",
    },
    config = true,
  },
  {
    "folke/sidekick.nvim",
    opts = {
      -- add any options here
      cli = {
        mux = {
          backend = "tmux",
          enabled = true,
        },
      },
    },
    -- stylua: ignore
    keys = {
      {
        "<tab>",
        function()
          -- if there is a next edit, jump to it, otherwise apply it if any
          if not require("sidekick").nes_jump_or_apply() then
            return "<Tab>" -- fallback to normal tab
          end
        end,
        expr = true,
        desc = "Goto/Apply Next Edit Suggestion",
      },
      {
        "<leader>aa",
        function() require("sidekick.cli").toggle() end,
        desc = "Sidekick Toggle CLI",
      },
      {
        "<leader>as",
        function() require("sidekick.cli").select() end,
        -- Or to select only installed tools:
        -- require("sidekick.cli").select({ filter = { installed = true } })
        desc = "Select CLI",
      },
      {
        "<leader>at",
        function() require("sidekick.cli").send({ msg = "{this}" }) end,
        mode = { "x", "n" },
        desc = "Send This",
      },
      {
        "<leader>av",
        function() require("sidekick.cli").send({ msg = "{selection}" }) end,
        mode = { "x" },
        desc = "Send Visual Selection",
      },
      {
        "<leader>ap",
        function() require("sidekick.cli").prompt() end,
        mode = { "n", "x" },
        desc = "Sidekick Select Prompt",
      },
      {
        "<c-.>",
        function() require("sidekick.cli").focus() end,
        mode = { "n", "x", "i", "t" },
        desc = "Sidekick Switch Focus",
      },
      -- Example of a keybinding to open Claude directly
      {
        "<leader>ac",
        function() require("sidekick.cli").toggle({ name = "claude", focus = true }) end,
        desc = "Sidekick Toggle Claude",
      },
    },
  },
}
