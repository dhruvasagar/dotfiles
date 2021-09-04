-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/home/h4x0rdud3/.cache/nvim/packer_hererocks/2.0.5/share/lua/5.1/?.lua;/home/h4x0rdud3/.cache/nvim/packer_hererocks/2.0.5/share/lua/5.1/?/init.lua;/home/h4x0rdud3/.cache/nvim/packer_hererocks/2.0.5/lib/luarocks/rocks-5.1/?.lua;/home/h4x0rdud3/.cache/nvim/packer_hererocks/2.0.5/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/h4x0rdud3/.cache/nvim/packer_hererocks/2.0.5/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s))
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  DrawIt = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/DrawIt"
  },
  SyntaxRange = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/SyntaxRange"
  },
  ale = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/ale"
  },
  ["coc.nvim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/coc.nvim"
  },
  ["editorconfig-vim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/editorconfig-vim"
  },
  ["gist-vim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/gist-vim"
  },
  ["glow.nvim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/glow.nvim"
  },
  ["goyo.vim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/goyo.vim"
  },
  ["haskell-vim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/haskell-vim"
  },
  ["indent-blankline.nvim"] = {
    config = { "\27LJ\1\2j\0\0\3\0\6\0\t4\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\4\0003\2\3\0:\2\5\1>\0\2\1G\0\1\0\20buftype_exclude\1\0\0\1\2\0\0\rterminal\nsetup\21indent_blankline\frequire\0" },
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/indent-blankline.nvim"
  },
  lspconfig = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/lspconfig"
  },
  ["lua-dev.nvim"] = {
    config = { "\27LJ\1\2k\0\0\4\0\5\0\0144\0\0\0%\1\1\0>\0\2\0027\0\2\0002\1\0\0>\0\2\0024\1\0\0%\2\3\0>\1\2\0027\2\4\0017\2\2\2\16\3\0\0>\2\2\1G\0\1\0\16sumneko_lua\14lspconfig\nsetup\flua-dev\frequire\0" },
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/lua-dev.nvim"
  },
  mkdx = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/mkdx"
  },
  nuake = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/nuake"
  },
  ["nvim-colorizer.lua"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/nvim-colorizer.lua"
  },
  ["nvim-dap"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/nvim-dap"
  },
  ["nvim-dap-ui"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/nvim-dap-ui"
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/nvim-lspconfig"
  },
  ["nvim-lspinstall"] = {
    config = { "\27LJ\1\2¢\1\0\0\b\0\6\0\0244\0\0\0%\1\1\0>\0\2\0027\0\2\0>\0\1\0014\0\0\0%\1\1\0>\0\2\0027\0\3\0>\0\1\0024\1\4\0\16\2\0\0>\1\2\4D\4\a€4\6\0\0%\a\5\0>\6\2\0026\6\5\0067\6\2\0062\a\0\0>\6\2\1B\4\3\3N\4÷G\0\1\0\14lspconfig\npairs\22installed_servers\nsetup\15lspinstall\frequire\21\0\0\1\1\0\0\3+\0\0\0>\0\1\1G\0\1\0\0ÀV\1\0\3\0\5\0\n1\0\0\0\16\1\0\0>\1\1\0014\1\1\0%\2\2\0>\1\2\0021\2\4\0:\2\3\0010\0\0€G\0\1\0\0\22post_install_hook\15lspinstall\frequire\0\0" },
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/nvim-lspinstall"
  },
  ["nvim-treesitter"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["octo.nvim"] = {
    config = { "\27LJ\1\0022\0\0\2\0\3\0\0064\0\0\0%\1\1\0>\0\2\0027\0\2\0>\0\1\1G\0\1\0\nsetup\tocto\frequire\0" },
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/octo.nvim"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["purescript-vim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/purescript-vim"
  },
  ["rust-tools.nvim"] = {
    config = { "\27LJ\1\2<\0\0\2\0\3\0\a4\0\0\0%\1\1\0>\0\2\0027\0\2\0002\1\0\0>\0\2\1G\0\1\0\nsetup\15rust-tools\frequire\0" },
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/rust-tools.nvim"
  },
  ["sideways.vim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/sideways.vim"
  },
  ["splitjoin.vim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/splitjoin.vim"
  },
  ["switch.vim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/switch.vim"
  },
  tabular = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/tabular"
  },
  ["targets.vim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/targets.vim"
  },
  ["telescope.nvim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/telescope.nvim"
  },
  ["typescript-vim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/typescript-vim"
  },
  ultisnips = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/ultisnips"
  },
  ["vim-abolish"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-abolish"
  },
  ["vim-angular"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-angular"
  },
  ["vim-apathy"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-apathy"
  },
  ["vim-buffer-history"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-buffer-history"
  },
  ["vim-bundler"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-bundler"
  },
  ["vim-characterize"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-characterize"
  },
  ["vim-clojure-static"] = {
    loaded = false,
    needs_bufread = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/opt/vim-clojure-static"
  },
  ["vim-closer"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-closer"
  },
  ["vim-commentary"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-commentary"
  },
  ["vim-comp"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-comp"
  },
  ["vim-cucumber"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-cucumber"
  },
  ["vim-dispatch"] = {
    commands = { "Dispatch", "Make", "Focus", "Start" },
    loaded = false,
    needs_bufread = false,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/opt/vim-dispatch"
  },
  ["vim-doge"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-doge"
  },
  ["vim-dotoo"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-dotoo"
  },
  ["vim-elixir"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-elixir"
  },
  ["vim-emoji"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-emoji"
  },
  ["vim-endwise"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-endwise"
  },
  ["vim-eunuch"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-eunuch"
  },
  ["vim-exchange"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-exchange"
  },
  ["vim-fireplace"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/opt/vim-fireplace"
  },
  ["vim-fugitive"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-fugitive"
  },
  ["vim-github-review"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-github-review"
  },
  ["vim-go"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-go"
  },
  ["vim-helm"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-helm"
  },
  ["vim-jade"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-jade"
  },
  ["vim-javascript"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-javascript"
  },
  ["vim-jsx-pretty"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-jsx-pretty"
  },
  ["vim-ledger"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-ledger"
  },
  ["vim-leiningen"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/opt/vim-leiningen"
  },
  ["vim-less"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-less"
  },
  ["vim-marp"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-marp"
  },
  ["vim-matchup"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-matchup"
  },
  ["vim-mundo"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-mundo"
  },
  ["vim-obsession"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-obsession"
  },
  ["vim-open-url"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-open-url"
  },
  ["vim-pairify"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-pairify"
  },
  ["vim-phoenix"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-phoenix"
  },
  ["vim-plugin-AnsiEsc"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-plugin-AnsiEsc"
  },
  ["vim-prosession"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/opt/vim-prosession"
  },
  ["vim-racer"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-racer"
  },
  ["vim-rails"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-rails"
  },
  ["vim-railscasts-theme"] = {
    config = { "\27LJ\1\2:\0\0\2\0\3\0\0054\0\0\0007\0\1\0%\1\2\0>\0\2\1G\0\1\0\27colorscheme railscasts\bcmd\bvim\0" },
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-railscasts-theme"
  },
  ["vim-rake"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-rake"
  },
  ["vim-repeat"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-repeat"
  },
  ["vim-rest-console"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-rest-console"
  },
  ["vim-rhubarb"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-rhubarb"
  },
  ["vim-rsi"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-rsi"
  },
  ["vim-sandwich"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-sandwich"
  },
  ["vim-scriptease"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-scriptease"
  },
  ["vim-sensible"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-sensible"
  },
  ["vim-sexp"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/opt/vim-sexp"
  },
  ["vim-sexp-mappings-for-regular-people"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/opt/vim-sexp-mappings-for-regular-people"
  },
  ["vim-slim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-slim"
  },
  ["vim-snippets"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-snippets"
  },
  ["vim-speeddating"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-speeddating"
  },
  ["vim-table-mode"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-table-mode"
  },
  ["vim-tbone"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-tbone"
  },
  ["vim-test"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-test"
  },
  ["vim-testify"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-testify"
  },
  ["vim-unimpaired"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-unimpaired"
  },
  ["vim-vspec"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-vspec"
  },
  ["vim-zoom"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vim-zoom"
  },
  vimspector = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vimspector"
  },
  vimux = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vimux"
  },
  vimwiki = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/vimwiki"
  },
  ["webapi-vim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/webapi-vim"
  },
  ["xterm-color-table.vim"] = {
    loaded = true,
    path = "/home/h4x0rdud3/.local/share/nvim/site/pack/packer/start/xterm-color-table.vim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: octo.nvim
time([[Config for octo.nvim]], true)
try_loadstring("\27LJ\1\0022\0\0\2\0\3\0\0064\0\0\0%\1\1\0>\0\2\0027\0\2\0>\0\1\1G\0\1\0\nsetup\tocto\frequire\0", "config", "octo.nvim")
time([[Config for octo.nvim]], false)
-- Config for: vim-railscasts-theme
time([[Config for vim-railscasts-theme]], true)
try_loadstring("\27LJ\1\2:\0\0\2\0\3\0\0054\0\0\0007\0\1\0%\1\2\0>\0\2\1G\0\1\0\27colorscheme railscasts\bcmd\bvim\0", "config", "vim-railscasts-theme")
time([[Config for vim-railscasts-theme]], false)
-- Config for: lua-dev.nvim
time([[Config for lua-dev.nvim]], true)
try_loadstring("\27LJ\1\2k\0\0\4\0\5\0\0144\0\0\0%\1\1\0>\0\2\0027\0\2\0002\1\0\0>\0\2\0024\1\0\0%\2\3\0>\1\2\0027\2\4\0017\2\2\2\16\3\0\0>\2\2\1G\0\1\0\16sumneko_lua\14lspconfig\nsetup\flua-dev\frequire\0", "config", "lua-dev.nvim")
time([[Config for lua-dev.nvim]], false)
-- Config for: rust-tools.nvim
time([[Config for rust-tools.nvim]], true)
try_loadstring("\27LJ\1\2<\0\0\2\0\3\0\a4\0\0\0%\1\1\0>\0\2\0027\0\2\0002\1\0\0>\0\2\1G\0\1\0\nsetup\15rust-tools\frequire\0", "config", "rust-tools.nvim")
time([[Config for rust-tools.nvim]], false)
-- Config for: indent-blankline.nvim
time([[Config for indent-blankline.nvim]], true)
try_loadstring("\27LJ\1\2j\0\0\3\0\6\0\t4\0\0\0%\1\1\0>\0\2\0027\0\2\0003\1\4\0003\2\3\0:\2\5\1>\0\2\1G\0\1\0\20buftype_exclude\1\0\0\1\2\0\0\rterminal\nsetup\21indent_blankline\frequire\0", "config", "indent-blankline.nvim")
time([[Config for indent-blankline.nvim]], false)
-- Config for: nvim-lspinstall
time([[Config for nvim-lspinstall]], true)
try_loadstring("\27LJ\1\2¢\1\0\0\b\0\6\0\0244\0\0\0%\1\1\0>\0\2\0027\0\2\0>\0\1\0014\0\0\0%\1\1\0>\0\2\0027\0\3\0>\0\1\0024\1\4\0\16\2\0\0>\1\2\4D\4\a€4\6\0\0%\a\5\0>\6\2\0026\6\5\0067\6\2\0062\a\0\0>\6\2\1B\4\3\3N\4÷G\0\1\0\14lspconfig\npairs\22installed_servers\nsetup\15lspinstall\frequire\21\0\0\1\1\0\0\3+\0\0\0>\0\1\1G\0\1\0\0ÀV\1\0\3\0\5\0\n1\0\0\0\16\1\0\0>\1\1\0014\1\1\0%\2\2\0>\1\2\0021\2\4\0:\2\3\0010\0\0€G\0\1\0\0\22post_install_hook\15lspinstall\frequire\0\0", "config", "nvim-lspinstall")
time([[Config for nvim-lspinstall]], false)

-- Command lazy-loads
time([[Defining lazy-load commands]], true)
pcall(vim.cmd, [[command! -nargs=* -range -bang -complete=file Make lua require("packer.load")({'vim-dispatch'}, { cmd = "Make", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command! -nargs=* -range -bang -complete=file Focus lua require("packer.load")({'vim-dispatch'}, { cmd = "Focus", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command! -nargs=* -range -bang -complete=file Dispatch lua require("packer.load")({'vim-dispatch'}, { cmd = "Dispatch", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
pcall(vim.cmd, [[command! -nargs=* -range -bang -complete=file Start lua require("packer.load")({'vim-dispatch'}, { cmd = "Start", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args> }, _G.packer_plugins)]])
time([[Defining lazy-load commands]], false)

if should_profile then save_profiles() end

end)

if not no_errors then
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
