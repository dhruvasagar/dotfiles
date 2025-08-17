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
    "yetone/avante.nvim",
    event = "VeryLazy",
    lazy = false,
    version = false, -- set this if you want to always pull the latest change
    opts = {
      provider = "openai",
    },
    -- if you want to build from source then do `make BUILD_FROM_SOURCE=true`
    build = "make",
    -- build = "powershell -ExecutionPolicy Bypass -File Build.ps1 -BuildFromSource false" -- for windows
    dependencies = {
      "stevearc/dressing.nvim",
      "nvim-lua/plenary.nvim",
      "MunifTanjim/nui.nvim",
      --- The below dependencies are optional,
      "nvim-tree/nvim-web-devicons", -- or echasnovski/mini.icons
      {
        -- support for image pasting
        "HakonHarnes/img-clip.nvim",
        event = "VeryLazy",
        opts = {
          -- recommended settings
          default = {
            embed_image_as_base64 = false,
            prompt_for_file_name = false,
            drag_and_drop = {
              insert_mode = true,
            },
            -- required for Windows users
            use_absolute_path = true,
          },
        },
      },
    },
  },
  {
    "olimorris/codecompanion.nvim",
    opts = {},
  },
}
