local generic = {
  surrounds = {
    ["g"] = {
      add = function()
        local cfg = require("nvim-surround.config")
        local result = cfg.get_input("Enter the generic name: ")
        if result then
          return { { result .. "<" }, { ">" } }
        end
      end,
      find = function()
        local cfg = require("nvim-surround.config")
        return cfg.get_selection({ node = "generic_type" })
      end,
      delete = "^(.-<)().-(>)()$",
      change = {
        target = "^(.-<)().-(>)()$",
        replacement = function()
          local cfg = require("nvim-surround.config")
          local result = cfg.get_input("Enter the generic name: ")
          if result then
            return { { result .. "<" }, { ">" } }
          end
        end,
      },
    },
  },
}

local tex_find_environment = function()
  local cfg = require("nvim-surround.config")
  if vim.g.loaded_nvim_treesitter then
    local selection = cfg.get_selection({
      node = "generic_environment",
      -- query = {
      --   capture = "@block.outer",
      --   type = "textobjects",
      -- }
      -- NOTE: ^query doesn't seem to work very reliably with LaTeX environments
    })
    if selection then
      return selection
    end
  end
  return cfg.get_selection([[\begin%b{}.-\end%b{}]])
  -- NOTE: ^this does not correctly handle \begin{}-\end{} pairs in all cases
  --        (hence we use treesitter if available)
end

local tex = {
  surrounds = {
    ["c"] = {
      add = function()
        local cfg = require("nvim-surround.config")
        local cmd = cfg.get_input("Command: ")
        return { { "\\" .. cmd .. "{" }, { "}" } }
      end,
      find = [=[\[^\{}%[%]]-%b{}]=],
      delete = [[^(\[^\{}]-{)().-(})()$]],
      change = {
        target = [[^\([^\{}]-)()%b{}()()$]],
        replacement = function()
          local cmd = cfg.get_input("Command: ")
          return { { cmd }, { "" } }
        end,
      },
    },
    ["C"] = {
      add = function()
        local cfg = require("nvim-surround.config")
        local cmd, opts = cfg.get_input("Command: "), cfg.get_input("Options: ")
        return { { "\\" .. cmd .. "[" .. opts .. "]{" }, { "}" } }
      end,
      find = [[\[^\{}]-%b[]%b{}]],
      delete = [[^(\[^\{}]-%b[]{)().-(})()$]],
      change = {
        target = [[^\([^\{}]-)()%[(.*)()%]%b{}$]],
        replacement = function()
          local cfg = require("nvim-surround.config")
          local cmd, opts = cfg.get_input("Command: "), cfg.get_input("Options: ")
          return { { cmd }, { opts } }
        end,
      },
    },
    ["e"] = {
      add = function()
        local cfg = require("nvim-surround.config")
        local env = cfg.get_input("Environment: ")
        return { { "\\begin{" .. env .. "}" }, { "\\end{" .. env .. "}" } }
      end,
      find = tex_find_environment,
      delete = [[^(\begin%b{})().*(\end%b{})()$]],
      change = {
        target = [[^\begin{(.-)()%}.*\end{(.-)()}$]],
        replacement = function()
          local env = require("nvim-surround.config").get_input("Environment: ")
          return { { env }, { env } }
        end,
      },
    },
    ["E"] = {
      add = function()
        local cfg = require("nvim-surround.config")
        local env, opts = cfg.get_input("Environment: "), cfg.get_input("Options: ")
        return { { "\\begin{" .. env .. "}[" .. opts .. "]" }, { "\\end{" .. env .. "}" } }
      end,
      find = tex_find_environment,
      delete = [[^(\begin%b{}%b[])().*(\end%b{})()$]],
      change = {
        target = [[^\begin%b{}%[(.-)()()()%].*\end%b{}$]],
        replacement = function()
          local cfg = require("nvim-surround.config")
          local env = cfg.get_input("Environment options: ")
          return { { env }, { "" } }
        end,
      },
    },
  },
}

return { generic = generic, tex = tex }
