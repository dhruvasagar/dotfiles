local dap, dapui = require("dap"), require("dapui")

require("dap.ext.vscode").load_launchjs()

dap.adapters.codelldb = {
  type = "server",
  port = "${port}",
  executable = {
    command = os.getenv("HOME") .. "/.local/share/nvim/mason/packages/codelldb/extension/adapter/codelldb",
    args = { "--port", "${port}" },
  },
}

dap.configurations.c = {
  {
    name = "Launch",
    type = "codelldb",
    request = "launch",
    program = function()
      return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
    end,
    cwd = "${workspaceFolder}",
    terminal = "integrated",
    stopOnEntry = false,
  },
}
dap.configurations.cpp = dap.configurations.c
dap.configurations.rust = dap.configurations.c

dap.adapters.chrome = {
  -- executable: launch the remote debug adapter - server: connect to an already running debug adapter
  type = "executable",
  -- command to launch the debug adapter - used only on executable type
  command = "node",
  args = { os.getenv("HOME") .. "/.local/share/nvim/mason/packages/chrome-debug-adapter/out/src/chromeDebug.js" },
}
-- The configuration must be named: typescript
dap.configurations.typescript = {
  {
    name = "Debug (Attach) - Remote",
    type = "chrome",
    request = "attach",
    -- program = "${file}",
    -- cwd = vim.fn.getcwd(),
    sourceMaps = true,
    --      reAttach = true,
    trace = true,
    -- protocol = "inspector",
    -- hostName = "127.0.0.1",
    port = "${port}",
    webRoot = "${workspaceFolder}",
  },
}

dap.configurations.lua = {
  {
    type = "nlua",
    request = "attach",
    name = "Attach to running Neovim instance",
  },
}

dap.adapters.nlua = function(callback, config)
  callback({ type = "server", host = config.host or "127.0.0.1", port = config.port or 8086 })
end

-- Configuration for Java
dap.adapters.java = function(callback)
  callback({ type = "server", host = "127.0.0.1", port = 8000 })
end
dap.configurations.java = {
  {
    type = "java",
    request = "attach",
    name = "Debug (Attach) - Remote",
    hostName = "127.0.0.1",
    port = 8000,
  },
}

-- dap.set_log_level("DEBUG")

dap.listeners.after.event_initialized["dapui_config"] = function()
  dapui.open()
end
dap.listeners.before.event_terminated["dapui_config"] = function()
  dapui.close()
end
dap.listeners.after.event_exited["dapui_config"] = function()
  dapui.close()
end

vim.keymap.set("n", "<F5>", function()
  require("dap").continue()
end, { desc = "dap.continue" })
vim.keymap.set("n", "<F10>", function()
  require("dap").step_over()
end, { desc = "dap.step_over" })
vim.keymap.set("n", "<F11>", function()
  require("dap").step_into()
end, { desc = "dap.step_into" })
vim.keymap.set("n", "<F12>", function()
  require("dap").step_out()
end, { desc = "dap.step_out" })
vim.keymap.set("n", "<Leader>b", function()
  require("dap").toggle_breakpoint()
end, { desc = "dap.toggle_breakpoint" })
vim.keymap.set("n", "<Leader>B", function()
  require("dap").set_breakpoint(vim.fn.input("Breakpoint condition: "))
end, { desc = "dap.set_breakpoint with condition" })
vim.keymap.set("n", "<Leader>lp", function()
  require("dap").set_breakpoint(nil, nil, vim.fn.input("Log point message: "))
end, { desc = "dap.set_breakpoint with log point message" })
vim.keymap.set("n", "<Leader>dr", function()
  require("dap").repl.open()
end, { desc = "dap.repl.open" })
vim.keymap.set("n", "<Leader>dl", function()
  require("dap").run_last()
end, { desc = "dap.run_last" })
vim.keymap.set("n", "<Leader>dc", function()
  require("dapui").close()
end, { desc = "dapui.close" })

vim.keymap.set({ "n", "v" }, "<Leader>dh", function()
  require("dap.ui.widgets").hover()
end, { desc = "dap.ui.widgets.hover" })

vim.keymap.set({ "n", "v" }, "<Leader>dp", function()
  require("dap.ui.widgets").preview()
end, { desc = "dap.ui.widgets.preview" })

vim.keymap.set("n", "<Leader>df", function()
  local widgets = require("dap.ui.widgets")
  widgets.centered_float(widgets.frames)
end, { desc = "dap.ui.widgets.frames" })

vim.keymap.set("n", "<Leader>ds", function()
  local widgets = require("dap.ui.widgets")
  widgets.centered_float(widgets.scopes)
end, { desc = "dap.ui.widgets.scopes" })
