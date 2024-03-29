local dap, dapui = require("dap"), require("dapui")

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
  args = { os.getenv("HOME") .. "/.local/share/nvim/mason/packages/chrome-debug-adapter/out/src/chromeDebug.js" }
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
    webRoot = "${workspaceFolder}"
  }
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

vim.keymap.set('n', '<F5>', function() require('dap').continue() end)
vim.keymap.set('n', '<F10>', function() require('dap').step_over() end)
vim.keymap.set('n', '<F11>', function() require('dap').step_into() end)
vim.keymap.set('n', '<F12>', function() require('dap').step_out() end)
vim.keymap.set('n', '<Leader>b', function() require('dap').toggle_breakpoint() end)
vim.keymap.set('n', '<Leader>B', function() require('dap').set_breakpoint(vim.fn.input('Breakpoint condition: ')) end)
vim.keymap.set('n', '<Leader>lp',
  function() require('dap').set_breakpoint(nil, nil, vim.fn.input('Log point message: ')) end)
vim.keymap.set('n', '<Leader>dr', function() require('dap').repl.open() end)
vim.keymap.set('n', '<Leader>dl', function() require('dap').run_last() end)

vim.keymap.set({ 'n', 'v' }, '<Leader>dh', function()
  require('dap.ui.widgets').hover()
end)

vim.keymap.set({ 'n', 'v' }, '<Leader>dp', function()
  require('dap.ui.widgets').preview()
end)

vim.keymap.set('n', '<Leader>df', function()
  local widgets = require('dap.ui.widgets')
  widgets.centered_float(widgets.frames)
end)

vim.keymap.set('n', '<Leader>ds', function()
  local widgets = require('dap.ui.widgets')
  widgets.centered_float(widgets.scopes)
end)
