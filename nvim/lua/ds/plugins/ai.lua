return {
  {
    "David-Kunz/gen.nvim",
    opts = {
      model = "mistral",
      display_mode = "split",
      show_prompt = true,
      show_model = true,
      no_auto_close = true,
    },
  },
  {
    "Exafunction/codeium.nvim",
    dependencies = { "nvim-lua/plenary.nvim", "hrsh7th/nvim-cmp" },
    config = true,
    opts = {
      tools = {
        language_server = "/usr/local/bin/language_server_macos_arm",
      },
    },
  },
  {
    "zbirenbaum/copilot.lua",
    enabled = false,
    cmd = "Copilot",
    event = "InsertEnter",
    config = true,
  },
  {
    "sourcegraph/sg.nvim",
    opts = {},
  },
}
