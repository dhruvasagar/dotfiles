return {
  {
    "David-Kunz/gen.nvim",
    opts = {
      model = "eramax/nxcode-cq-7b-orpo:q6", -- The default model to use.
      quit_map = "q",                     -- set keymap for close the response window
      retry_map = "<c-r>",                -- set keymap to re-send the current prompt
      accept_map = "<c-cr>",              -- set keymap to replace the previous selection with the last result
      host = "localhost",                 -- The host running the Ollama service.
      port = "11434",                     -- The port on which the Ollama service is listening.
      display_mode = "float",             -- The display mode. Can be "float" or "split" or "horizontal-split".
      show_prompt = false,                -- Shows the prompt submitted to Ollama.
      show_model = false,                 -- Displays which model you are using at the beginning of your chat session.
      no_auto_close = false,              -- Never closes the window automatically.
      hidden = false,                     -- Hide the generation window (if true, will implicitly set `prompt.replace = true`), requires Neovim >= 0.10
      init = function(options)
        pcall(io.popen, "ollama serve > /dev/null 2>&1 &")
      end,
      -- Function to initialize Ollama
      command = function(options)
        local body = { model = options.model, stream = true }
        return "curl --silent --no-buffer -X POST http://"
            .. options.host
            .. ":"
            .. options.port
            .. "/api/chat -d $body"
      end,
      -- The command for the Ollama service. You can use placeholders $prompt, $model and $body (shellescaped).
      -- This can also be a command string.
      -- The executed command must return a JSON object with { response, context }
      -- (context property is optional).
      -- list_models = '<omitted lua function>', -- Retrieves a list of model names
      debug = false, -- Prints errors and the command which is run.
    },
  },
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
    "sourcegraph/sg.nvim",
    opts = {},
  },
  {
    "nomnivore/ollama.nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
    },
    ---@type Ollama.Config
    opts = {
      -- your configuration overrides
    },
  },
  {
    "robitx/gp.nvim",
    opts = {},
  },
  {
    "olimorris/codecompanion.nvim",
    opts = {},
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
}
