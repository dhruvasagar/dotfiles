-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

_G._packer = _G._packer or {}
_G._packer.inside_compile = true

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
  if threshold then
    table.insert(results, '(Only showing plugins that took longer than ' .. threshold .. ' ms ' .. 'to load)')
  end

  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/Users/dhruva/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/Users/dhruva/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/Users/dhruva/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/Users/dhruva/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/Users/dhruva/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s), name, _G.packer_plugins[name])
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
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/DrawIt",
    url = "https://github.com/vim-scripts/DrawIt"
  },
  SyntaxRange = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/SyntaxRange",
    url = "https://github.com/vim-scripts/SyntaxRange"
  },
  ["cmp-buffer"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/cmp-buffer",
    url = "https://github.com/hrsh7th/cmp-buffer"
  },
  ["cmp-digraphs"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/cmp-digraphs",
    url = "https://github.com/dmitmel/cmp-digraphs"
  },
  ["cmp-git"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/cmp-git",
    url = "https://github.com/petertriho/cmp-git"
  },
  ["cmp-npm"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/cmp-npm",
    url = "https://github.com/David-Kunz/cmp-npm"
  },
  ["cmp-nvim-lsp"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/cmp-nvim-lsp",
    url = "https://github.com/hrsh7th/cmp-nvim-lsp"
  },
  ["cmp-nvim-lsp-signature-help"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/cmp-nvim-lsp-signature-help",
    url = "https://github.com/hrsh7th/cmp-nvim-lsp-signature-help"
  },
  ["cmp-nvim-ultisnips"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/cmp-nvim-ultisnips",
    url = "https://github.com/quangnguyen30192/cmp-nvim-ultisnips"
  },
  ["cmp-omni"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/cmp-omni",
    url = "https://github.com/hrsh7th/cmp-omni"
  },
  ["cmp-path"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/cmp-path",
    url = "https://github.com/hrsh7th/cmp-path"
  },
  ["crates.nvim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/crates.nvim",
    url = "https://github.com/Saecki/crates.nvim"
  },
  ["editorconfig-vim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/editorconfig-vim",
    url = "https://github.com/editorconfig/editorconfig-vim"
  },
  ["gist-vim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/gist-vim",
    url = "https://github.com/mattn/gist-vim"
  },
  ["glow.nvim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/glow.nvim",
    url = "https://github.com/ellisonleao/glow.nvim"
  },
  ["goyo.vim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/goyo.vim",
    url = "https://github.com/junegunn/goyo.vim"
  },
  ["haskell-vim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/haskell-vim",
    url = "https://github.com/neovimhaskell/haskell-vim"
  },
  ["indent-blankline.nvim"] = {
    config = { "\27LJ\2\n—\1\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\1K\0\1\0\20buftype_exclude\1\0\2\25space_char_blankline\6 \25show_current_context\2\1\2\0\0\rterminal\nsetup\21indent_blankline\frequire\0" },
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/indent-blankline.nvim",
    url = "https://github.com/lukas-reineke/indent-blankline.nvim"
  },
  ["lsp-zero.nvim"] = {
    config = { "\27LJ\2\nR\0\0\4\0\5\0\t6\0\0\0'\2\1\0B\0\2\0029\1\2\0'\3\3\0B\1\2\0019\1\4\0B\1\1\1K\0\1\0\nsetup\rlsp-only\vpreset\rlsp-zero\frequire\0" },
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/lsp-zero.nvim",
    url = "https://github.com/VonHeikemen/lsp-zero.nvim"
  },
  ["mason-lspconfig.nvim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/mason-lspconfig.nvim",
    url = "https://github.com/williamboman/mason-lspconfig.nvim"
  },
  ["mason.nvim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/mason.nvim",
    url = "https://github.com/williamboman/mason.nvim"
  },
  mkdx = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/mkdx",
    url = "https://github.com/SidOfc/mkdx"
  },
  ["neodev.nvim"] = {
    config = { "\27LJ\2\n8\0\0\3\0\3\0\a6\0\0\0'\2\1\0B\0\2\0029\0\2\0004\2\0\0B\0\2\1K\0\1\0\nsetup\vneodev\frequire\0" },
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/neodev.nvim",
    url = "https://github.com/folke/neodev.nvim"
  },
  nuake = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/nuake",
    url = "https://github.com/Lenovsky/nuake"
  },
  ["null-ls.nvim"] = {
    config = { "\27LJ\2\nI\0\0\4\1\6\0\t6\0\0\0009\0\1\0009\0\2\0009\0\3\0005\2\4\0-\3\0\0=\3\5\2B\0\2\1K\0\1\0\1À\nbufnr\1\0\0\vformat\bbuf\blsp\bvimò\1\1\2\a\1\r\0\0269\2\0\0'\4\1\0B\2\2\2\15\0\2\0X\3\19€6\2\2\0009\2\3\0029\2\4\0025\4\5\0-\5\0\0=\5\6\4=\1\a\4B\2\2\0016\2\2\0009\2\3\0029\2\b\2'\4\t\0005\5\n\0-\6\0\0=\6\6\5=\1\a\0053\6\v\0=\6\f\5B\2\3\0012\0\0€K\0\1\0\3À\rcallback\0\1\0\0\16BufWritePre\24nvim_create_autocmd\vbuffer\ngroup\1\0\0\24nvim_clear_autocmds\bapi\bvim\28textDocument/formatting\20supports_methodÅ\4\1\0\f\0!\1;6\0\0\0'\2\1\0B\0\2\0029\1\2\0009\1\3\0019\2\2\0009\2\4\0026\3\5\0009\3\6\0039\3\a\3'\5\b\0004\6\0\0B\3\3\0029\4\t\0005\6\29\0004\a\f\0009\b\n\0019\b\v\b5\n\r\0005\v\f\0=\v\14\nB\b\2\2>\b\1\a9\b\15\1>\b\2\a9\b\16\1>\b\3\a9\b\17\1>\b\4\a9\b\18\1>\b\5\a9\b\19\1>\b\6\a9\b\20\1>\b\a\a9\b\21\0019\b\v\b5\n\23\0005\v\22\0=\v\24\nB\b\2\2>\b\b\a9\b\16\2>\b\t\a9\b\25\2>\b\n\a9\b\26\0029\b\v\b5\n\28\0005\v\27\0=\v\24\nB\b\2\0?\b\0\0=\a\30\0063\a\31\0=\a \6B\4\2\0012\0\0€K\0\1\0\14on_attach\0\fsources\1\0\0\1\0\0\1\5\0\0\14--globals\bvim\n--std\vluajit\rluacheck\15shellcheck\15extra_args\1\0\0\1\5\0\0\a-i\3\4\b-ci\b-sr\nshfmt\14goimports\vstylua\rfourmolu\frustfmt\reslint_d\14prettierd\14filetypes\1\0\0\1\b\0\0\ttext\ash\bzsh\ttoml\tmake\tconf\ttmux\twith\20trim_whitespace\nsetup\18LspFormatting\24nvim_create_augroup\bapi\bvim\16diagnostics\15formatting\rbuiltins\fnull-ls\frequire\23€€À™\4\0" },
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/null-ls.nvim",
    url = "https://github.com/jose-elias-alvarez/null-ls.nvim"
  },
  ["nvim-cmp"] = {
    config = { "\27LJ\2\n:\0\1\4\0\4\0\0066\1\0\0009\1\1\0019\1\2\0019\3\3\0B\1\2\1K\0\1\0\tbody\19UltiSnips#Anon\afn\bvimõ\1\0\1\a\2\v\2)6\1\0\0009\1\1\0019\1\2\1'\3\3\0B\1\2\2\23\1\0\1-\2\0\0009\2\4\2B\2\1\2\15\0\2\0X\3\5€-\2\0\0009\2\5\2-\4\1\0B\2\2\1X\2\24€\b\1\1\0X\2\16€6\2\0\0009\2\1\0029\2\6\2'\4\3\0B\2\2\2\18\4\2\0009\2\a\2\18\5\1\0\18\6\1\0B\2\4\2\18\4\2\0009\2\b\2'\5\t\0B\2\3\2\15\0\2\0X\3\3€\18\2\0\0B\2\1\1X\2\3€-\2\0\0009\2\n\2B\2\1\1K\0\1\0\0À\1À\rcomplete\a%s\nmatch\bsub\fgetline\21select_next_item\fvisible\6.\bcol\afn\bvim\2\0X\0\1\4\2\2\0\r-\1\0\0009\1\0\1B\1\1\2\15\0\1\0X\2\5€-\1\0\0009\1\1\1-\3\1\0B\1\2\1X\1\2€\18\1\0\0B\1\1\1K\0\1\0\0À\1À\21select_prev_item\fvisible¿\a\1\0\v\0007\0c6\0\0\0'\2\1\0B\0\2\0025\1\4\0009\2\2\0009\2\3\2=\2\5\0016\2\6\0009\2\a\2'\3\t\0=\3\b\0029\2\n\0005\4\14\0005\5\f\0003\6\v\0=\6\r\5=\5\15\0044\5\a\0005\6\16\0>\6\1\0055\6\17\0>\6\2\0055\6\18\0>\6\3\0055\6\19\0>\6\4\0055\6\20\0>\6\5\0055\6\21\0>\6\6\5=\5\22\0045\5\26\0009\6\23\0009\6\24\0065\b\25\0B\6\2\2=\6\27\0059\6\23\0009\6\28\6)\b\5\0B\6\2\2=\6\29\0059\6\23\0009\6\28\6)\bûÿB\6\2\2=\6\30\0059\6\23\0009\6\31\6B\6\1\2=\6 \0059\6\23\0009\6!\6\18\b\1\0B\6\2\2=\6\"\0059\6\23\0009\6#\6\18\b\1\0B\6\2\2=\6$\0059\6\23\0003\b%\0005\t&\0B\6\3\2=\6'\0059\6\23\0003\b(\0005\t)\0B\6\3\2=\6*\5=\5\23\0045\0051\0006\6\6\0009\6+\6'\b,\0009\t-\0009\t.\t9\t/\tB\t\1\0025\n0\0B\6\4\2=\0062\5=\5.\4B\2\2\0019\2\n\0009\0023\2'\0044\0005\0056\0009\6-\0009\6\22\0064\b\3\0005\t5\0>\t\1\bB\6\2\2=\6\22\5B\2\3\0012\0\0€K\0\1\0\1\0\0\1\0\1\tname\bgit\14gitcommit\rfiletype\18documentation\1\0\0\1\0\2\14max_width\3<\15max_height\3\15\rbordered\vwindow\vconfig\nforce\20tbl_deep_extend\f<S-Tab>\1\3\0\0\6i\6s\0\n<Tab>\1\3\0\0\6i\6s\0\n<C-n>\21select_next_item\n<C-p>\21select_prev_item\n<C-e>\nabort\n<C-u>\n<C-f>\16scroll_docs\t<CR>\1\0\0\1\0\1\vselect\2\fconfirm\fmapping\fsources\1\0\2\tname\28nvim_lsp_signature_help\19keyword_length\3\3\1\0\2\tname\rnvim_lsp\19keyword_length\3\3\1\0\2\tname\vbuffer\19keyword_length\3\3\1\0\1\tname\tomni\1\0\1\tname\tpath\1\0\1\tname\14ultisnips\fsnippet\1\0\0\vexpand\1\0\0\0\nsetup\26menu,menuone,noselect\16completeopt\bopt\bvim\rbehavior\1\0\0\vSelect\19SelectBehavior\bcmp\frequire\0" },
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/nvim-cmp",
    url = "https://github.com/hrsh7th/nvim-cmp"
  },
  ["nvim-colorizer.lua"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/nvim-colorizer.lua",
    url = "https://github.com/norcalli/nvim-colorizer.lua"
  },
  ["nvim-dap"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/nvim-dap",
    url = "https://github.com/mfussenegger/nvim-dap"
  },
  ["nvim-dap-ui"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/nvim-dap-ui",
    url = "https://github.com/rcarriga/nvim-dap-ui"
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/nvim-lspconfig",
    url = "https://github.com/neovim/nvim-lspconfig"
  },
  ["nvim-treesitter"] = {
    config = { "\27LJ\2\nî\2\0\0\5\0\14\0\0206\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\0025\1\a\0005\2\4\0005\3\5\0=\3\6\2=\2\b\1=\1\3\0006\1\0\0'\3\t\0B\1\2\0029\1\n\0015\3\f\0005\4\v\0=\4\r\3B\1\2\1K\0\1\0\21ensure_installed\1\0\0\1\t\0\0\tnorg\fhaskell\bcpp\6c\15javascript\trust\15typescript\ago\nsetup\28nvim-treesitter.configs\17install_info\1\0\0\nfiles\1\3\0\0\17src/parser.c\19src/scanner.cc\1\0\2\burl3https://github.com/nvim-neorg/tree-sitter-norg\vbranch\tmain\tnorg\23get_parser_configs\28nvim-treesitter.parsers\frequire\0" },
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/nvim-treesitter",
    url = "https://github.com/nvim-treesitter/nvim-treesitter"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/nvim-web-devicons",
    url = "https://github.com/kyazdani42/nvim-web-devicons"
  },
  ["octo.nvim"] = {
    config = { "\27LJ\2\n2\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\tocto\frequire\0" },
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/octo.nvim",
    url = "https://github.com/pwntester/octo.nvim"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/packer.nvim",
    url = "https://github.com/wbthomason/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/plenary.nvim",
    url = "https://github.com/nvim-lua/plenary.nvim"
  },
  ["purescript-vim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/purescript-vim",
    url = "https://github.com/purescript-contrib/purescript-vim"
  },
  rigel = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/rigel",
    url = "https://github.com/Rigellute/rigel"
  },
  ["sideways.vim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/sideways.vim",
    url = "https://github.com/AndrewRadev/sideways.vim"
  },
  ["splitjoin.vim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/splitjoin.vim",
    url = "https://github.com/AndrewRadev/splitjoin.vim"
  },
  ["switch.vim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/switch.vim",
    url = "https://github.com/AndrewRadev/switch.vim"
  },
  tabular = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/tabular",
    url = "https://github.com/godlygeek/tabular"
  },
  ["targets.vim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/targets.vim",
    url = "https://github.com/wellle/targets.vim"
  },
  ["telescope.nvim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/telescope.nvim",
    url = "https://github.com/nvim-telescope/telescope.nvim"
  },
  ["typescript-vim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/typescript-vim",
    url = "https://github.com/leafgarland/typescript-vim"
  },
  ultisnips = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/ultisnips",
    url = "https://github.com/SirVer/ultisnips"
  },
  ["vim-abolish"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-abolish",
    url = "https://github.com/tpope/vim-abolish"
  },
  ["vim-angular"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-angular",
    url = "https://github.com/burnettk/vim-angular"
  },
  ["vim-apathy"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-apathy",
    url = "https://github.com/tpope/vim-apathy"
  },
  ["vim-buffer-history"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-buffer-history",
    url = "/Users/dhruva/dotfiles/vim/pack/packup/start/vim-buffer-history"
  },
  ["vim-bundler"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-bundler",
    url = "https://github.com/tpope/vim-bundler"
  },
  ["vim-characterize"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-characterize",
    url = "https://github.com/tpope/vim-characterize"
  },
  ["vim-clojure-static"] = {
    loaded = false,
    needs_bufread = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/opt/vim-clojure-static",
    url = "https://github.com/guns/vim-clojure-static"
  },
  ["vim-commentary"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-commentary",
    url = "https://github.com/tpope/vim-commentary"
  },
  ["vim-comp"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-comp",
    url = "/Users/dhruva/dotfiles/vim/pack/packup/start/vim-comp"
  },
  ["vim-cucumber"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-cucumber",
    url = "https://github.com/tpope/vim-cucumber"
  },
  ["vim-dispatch"] = {
    commands = { "Dispatch", "Make", "Focus", "Start" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/opt/vim-dispatch",
    url = "https://github.com/tpope/vim-dispatch"
  },
  ["vim-doge"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-doge",
    url = "https://github.com/kkoomen/vim-doge"
  },
  ["vim-dotoo"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-dotoo",
    url = "/Users/dhruva/dotfiles/vim/pack/packup/start/vim-dotoo"
  },
  ["vim-elixir"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-elixir",
    url = "https://github.com/elixir-lang/vim-elixir"
  },
  ["vim-emoji"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-emoji",
    url = "https://github.com/junegunn/vim-emoji"
  },
  ["vim-endwise"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-endwise",
    url = "https://github.com/tpope/vim-endwise"
  },
  ["vim-eunuch"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-eunuch",
    url = "https://github.com/tpope/vim-eunuch"
  },
  ["vim-exchange"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-exchange",
    url = "https://github.com/tommcdo/vim-exchange"
  },
  ["vim-fireplace"] = {
    loaded = false,
    needs_bufread = false,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/opt/vim-fireplace",
    url = "https://github.com/tpope/vim-fireplace"
  },
  ["vim-fugitive"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-fugitive",
    url = "https://github.com/tpope/vim-fugitive"
  },
  ["vim-github-review"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-github-review",
    url = "/Users/dhruva/dotfiles/vim/pack/packup/start/vim-github-review"
  },
  ["vim-helm"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-helm",
    url = "https://github.com/towolf/vim-helm"
  },
  ["vim-jade"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-jade",
    url = "https://github.com/digitaltoad/vim-jade"
  },
  ["vim-javascript"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-javascript",
    url = "https://github.com/pangloss/vim-javascript"
  },
  ["vim-jsx-pretty"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-jsx-pretty",
    url = "https://github.com/MaxMEllon/vim-jsx-pretty"
  },
  ["vim-ledger"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-ledger",
    url = "https://github.com/ledger/vim-ledger"
  },
  ["vim-leiningen"] = {
    loaded = false,
    needs_bufread = false,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/opt/vim-leiningen",
    url = "https://github.com/tpope/vim-leiningen"
  },
  ["vim-less"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-less",
    url = "/Users/dhruva/dotfiles/vim/pack/packup/start/vim-less"
  },
  ["vim-marp"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-marp",
    url = "/Users/dhruva/dotfiles/vim/pack/packup/start/vim-marp"
  },
  ["vim-matchup"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-matchup",
    url = "https://github.com/andymass/vim-matchup"
  },
  ["vim-mundo"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-mundo",
    url = "https://github.com/simnalamburt/vim-mundo"
  },
  ["vim-mustache-handlebars"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-mustache-handlebars",
    url = "https://github.com/mustache/vim-mustache-handlebars"
  },
  ["vim-obsession"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-obsession",
    url = "https://github.com/tpope/vim-obsession"
  },
  ["vim-open-url"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-open-url",
    url = "/Users/dhruva/dotfiles/vim/pack/packup/start/vim-open-url"
  },
  ["vim-pairify"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-pairify",
    url = "/Users/dhruva/dotfiles/vim/pack/packup/start/vim-pairify"
  },
  ["vim-phoenix"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-phoenix",
    url = "https://github.com/avdgaag/vim-phoenix"
  },
  ["vim-plugin-AnsiEsc"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-plugin-AnsiEsc",
    url = "https://github.com/powerman/vim-plugin-AnsiEsc"
  },
  ["vim-prosession"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-prosession",
    url = "/Users/dhruva/dotfiles/vim/pack/packup/opt/vim-prosession"
  },
  ["vim-rails"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-rails",
    url = "https://github.com/tpope/vim-rails"
  },
  ["vim-railscasts-theme"] = {
    config = { "\27LJ\2\n:\0\0\3\0\3\0\0056\0\0\0009\0\1\0'\2\2\0B\0\2\1K\0\1\0\27colorscheme railscasts\bcmd\bvim\0" },
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-railscasts-theme",
    url = "/Users/dhruva/dotfiles/vim/pack/packup/start/vim-railscasts-theme"
  },
  ["vim-rake"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-rake",
    url = "https://github.com/tpope/vim-rake"
  },
  ["vim-repeat"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-repeat",
    url = "https://github.com/tpope/vim-repeat"
  },
  ["vim-rest-console"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-rest-console",
    url = "https://github.com/diepm/vim-rest-console"
  },
  ["vim-rhubarb"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-rhubarb",
    url = "https://github.com/tpope/vim-rhubarb"
  },
  ["vim-rsi"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-rsi",
    url = "https://github.com/tpope/vim-rsi"
  },
  ["vim-sandwich"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-sandwich",
    url = "https://github.com/machakann/vim-sandwich"
  },
  ["vim-scriptease"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-scriptease",
    url = "https://github.com/tpope/vim-scriptease"
  },
  ["vim-sensible"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-sensible",
    url = "https://github.com/tpope/vim-sensible"
  },
  ["vim-sexp"] = {
    loaded = false,
    needs_bufread = false,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/opt/vim-sexp",
    url = "https://github.com/guns/vim-sexp"
  },
  ["vim-sexp-mappings-for-regular-people"] = {
    loaded = false,
    needs_bufread = false,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/opt/vim-sexp-mappings-for-regular-people",
    url = "https://github.com/tpope/vim-sexp-mappings-for-regular-people"
  },
  ["vim-slim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-slim",
    url = "https://github.com/slim-template/vim-slim"
  },
  ["vim-snippets"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-snippets",
    url = "https://github.com/honza/vim-snippets"
  },
  ["vim-speeddating"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-speeddating",
    url = "https://github.com/tpope/vim-speeddating"
  },
  ["vim-startuptime"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-startuptime",
    url = "https://github.com/dstein64/vim-startuptime"
  },
  ["vim-table-mode"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-table-mode",
    url = "/Users/dhruva/dotfiles/vim/pack/packup/start/vim-table-mode"
  },
  ["vim-tbone"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-tbone",
    url = "https://github.com/tpope/vim-tbone"
  },
  ["vim-test"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-test",
    url = "https://github.com/janko-m/vim-test"
  },
  ["vim-testify"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-testify",
    url = "/Users/dhruva/dotfiles/vim/pack/packup/start/vim-testify"
  },
  ["vim-unimpaired"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-unimpaired",
    url = "https://github.com/tpope/vim-unimpaired"
  },
  ["vim-vspec"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-vspec",
    url = "https://github.com/kana/vim-vspec"
  },
  ["vim-zoom"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vim-zoom",
    url = "/Users/dhruva/dotfiles/vim/pack/packup/start/vim-zoom"
  },
  vimux = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/vimux",
    url = "https://github.com/benmills/vimux"
  },
  ["webapi-vim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/webapi-vim",
    url = "https://github.com/mattn/webapi-vim"
  },
  ["xterm-color-table.vim"] = {
    loaded = true,
    path = "/Users/dhruva/.local/share/nvim/site/pack/packer/start/xterm-color-table.vim",
    url = "https://github.com/guns/xterm-color-table.vim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: indent-blankline.nvim
time([[Config for indent-blankline.nvim]], true)
try_loadstring("\27LJ\2\n—\1\0\0\4\0\6\0\t6\0\0\0'\2\1\0B\0\2\0029\0\2\0005\2\4\0005\3\3\0=\3\5\2B\0\2\1K\0\1\0\20buftype_exclude\1\0\2\25space_char_blankline\6 \25show_current_context\2\1\2\0\0\rterminal\nsetup\21indent_blankline\frequire\0", "config", "indent-blankline.nvim")
time([[Config for indent-blankline.nvim]], false)
-- Config for: neodev.nvim
time([[Config for neodev.nvim]], true)
try_loadstring("\27LJ\2\n8\0\0\3\0\3\0\a6\0\0\0'\2\1\0B\0\2\0029\0\2\0004\2\0\0B\0\2\1K\0\1\0\nsetup\vneodev\frequire\0", "config", "neodev.nvim")
time([[Config for neodev.nvim]], false)
-- Config for: vim-railscasts-theme
time([[Config for vim-railscasts-theme]], true)
try_loadstring("\27LJ\2\n:\0\0\3\0\3\0\0056\0\0\0009\0\1\0'\2\2\0B\0\2\1K\0\1\0\27colorscheme railscasts\bcmd\bvim\0", "config", "vim-railscasts-theme")
time([[Config for vim-railscasts-theme]], false)
-- Config for: octo.nvim
time([[Config for octo.nvim]], true)
try_loadstring("\27LJ\2\n2\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\tocto\frequire\0", "config", "octo.nvim")
time([[Config for octo.nvim]], false)
-- Config for: lsp-zero.nvim
time([[Config for lsp-zero.nvim]], true)
try_loadstring("\27LJ\2\nR\0\0\4\0\5\0\t6\0\0\0'\2\1\0B\0\2\0029\1\2\0'\3\3\0B\1\2\0019\1\4\0B\1\1\1K\0\1\0\nsetup\rlsp-only\vpreset\rlsp-zero\frequire\0", "config", "lsp-zero.nvim")
time([[Config for lsp-zero.nvim]], false)
-- Config for: nvim-cmp
time([[Config for nvim-cmp]], true)
try_loadstring("\27LJ\2\n:\0\1\4\0\4\0\0066\1\0\0009\1\1\0019\1\2\0019\3\3\0B\1\2\1K\0\1\0\tbody\19UltiSnips#Anon\afn\bvimõ\1\0\1\a\2\v\2)6\1\0\0009\1\1\0019\1\2\1'\3\3\0B\1\2\2\23\1\0\1-\2\0\0009\2\4\2B\2\1\2\15\0\2\0X\3\5€-\2\0\0009\2\5\2-\4\1\0B\2\2\1X\2\24€\b\1\1\0X\2\16€6\2\0\0009\2\1\0029\2\6\2'\4\3\0B\2\2\2\18\4\2\0009\2\a\2\18\5\1\0\18\6\1\0B\2\4\2\18\4\2\0009\2\b\2'\5\t\0B\2\3\2\15\0\2\0X\3\3€\18\2\0\0B\2\1\1X\2\3€-\2\0\0009\2\n\2B\2\1\1K\0\1\0\0À\1À\rcomplete\a%s\nmatch\bsub\fgetline\21select_next_item\fvisible\6.\bcol\afn\bvim\2\0X\0\1\4\2\2\0\r-\1\0\0009\1\0\1B\1\1\2\15\0\1\0X\2\5€-\1\0\0009\1\1\1-\3\1\0B\1\2\1X\1\2€\18\1\0\0B\1\1\1K\0\1\0\0À\1À\21select_prev_item\fvisible¿\a\1\0\v\0007\0c6\0\0\0'\2\1\0B\0\2\0025\1\4\0009\2\2\0009\2\3\2=\2\5\0016\2\6\0009\2\a\2'\3\t\0=\3\b\0029\2\n\0005\4\14\0005\5\f\0003\6\v\0=\6\r\5=\5\15\0044\5\a\0005\6\16\0>\6\1\0055\6\17\0>\6\2\0055\6\18\0>\6\3\0055\6\19\0>\6\4\0055\6\20\0>\6\5\0055\6\21\0>\6\6\5=\5\22\0045\5\26\0009\6\23\0009\6\24\0065\b\25\0B\6\2\2=\6\27\0059\6\23\0009\6\28\6)\b\5\0B\6\2\2=\6\29\0059\6\23\0009\6\28\6)\bûÿB\6\2\2=\6\30\0059\6\23\0009\6\31\6B\6\1\2=\6 \0059\6\23\0009\6!\6\18\b\1\0B\6\2\2=\6\"\0059\6\23\0009\6#\6\18\b\1\0B\6\2\2=\6$\0059\6\23\0003\b%\0005\t&\0B\6\3\2=\6'\0059\6\23\0003\b(\0005\t)\0B\6\3\2=\6*\5=\5\23\0045\0051\0006\6\6\0009\6+\6'\b,\0009\t-\0009\t.\t9\t/\tB\t\1\0025\n0\0B\6\4\2=\0062\5=\5.\4B\2\2\0019\2\n\0009\0023\2'\0044\0005\0056\0009\6-\0009\6\22\0064\b\3\0005\t5\0>\t\1\bB\6\2\2=\6\22\5B\2\3\0012\0\0€K\0\1\0\1\0\0\1\0\1\tname\bgit\14gitcommit\rfiletype\18documentation\1\0\0\1\0\2\14max_width\3<\15max_height\3\15\rbordered\vwindow\vconfig\nforce\20tbl_deep_extend\f<S-Tab>\1\3\0\0\6i\6s\0\n<Tab>\1\3\0\0\6i\6s\0\n<C-n>\21select_next_item\n<C-p>\21select_prev_item\n<C-e>\nabort\n<C-u>\n<C-f>\16scroll_docs\t<CR>\1\0\0\1\0\1\vselect\2\fconfirm\fmapping\fsources\1\0\2\tname\28nvim_lsp_signature_help\19keyword_length\3\3\1\0\2\tname\rnvim_lsp\19keyword_length\3\3\1\0\2\tname\vbuffer\19keyword_length\3\3\1\0\1\tname\tomni\1\0\1\tname\tpath\1\0\1\tname\14ultisnips\fsnippet\1\0\0\vexpand\1\0\0\0\nsetup\26menu,menuone,noselect\16completeopt\bopt\bvim\rbehavior\1\0\0\vSelect\19SelectBehavior\bcmp\frequire\0", "config", "nvim-cmp")
time([[Config for nvim-cmp]], false)
-- Config for: null-ls.nvim
time([[Config for null-ls.nvim]], true)
try_loadstring("\27LJ\2\nI\0\0\4\1\6\0\t6\0\0\0009\0\1\0009\0\2\0009\0\3\0005\2\4\0-\3\0\0=\3\5\2B\0\2\1K\0\1\0\1À\nbufnr\1\0\0\vformat\bbuf\blsp\bvimò\1\1\2\a\1\r\0\0269\2\0\0'\4\1\0B\2\2\2\15\0\2\0X\3\19€6\2\2\0009\2\3\0029\2\4\0025\4\5\0-\5\0\0=\5\6\4=\1\a\4B\2\2\0016\2\2\0009\2\3\0029\2\b\2'\4\t\0005\5\n\0-\6\0\0=\6\6\5=\1\a\0053\6\v\0=\6\f\5B\2\3\0012\0\0€K\0\1\0\3À\rcallback\0\1\0\0\16BufWritePre\24nvim_create_autocmd\vbuffer\ngroup\1\0\0\24nvim_clear_autocmds\bapi\bvim\28textDocument/formatting\20supports_methodÅ\4\1\0\f\0!\1;6\0\0\0'\2\1\0B\0\2\0029\1\2\0009\1\3\0019\2\2\0009\2\4\0026\3\5\0009\3\6\0039\3\a\3'\5\b\0004\6\0\0B\3\3\0029\4\t\0005\6\29\0004\a\f\0009\b\n\0019\b\v\b5\n\r\0005\v\f\0=\v\14\nB\b\2\2>\b\1\a9\b\15\1>\b\2\a9\b\16\1>\b\3\a9\b\17\1>\b\4\a9\b\18\1>\b\5\a9\b\19\1>\b\6\a9\b\20\1>\b\a\a9\b\21\0019\b\v\b5\n\23\0005\v\22\0=\v\24\nB\b\2\2>\b\b\a9\b\16\2>\b\t\a9\b\25\2>\b\n\a9\b\26\0029\b\v\b5\n\28\0005\v\27\0=\v\24\nB\b\2\0?\b\0\0=\a\30\0063\a\31\0=\a \6B\4\2\0012\0\0€K\0\1\0\14on_attach\0\fsources\1\0\0\1\0\0\1\5\0\0\14--globals\bvim\n--std\vluajit\rluacheck\15shellcheck\15extra_args\1\0\0\1\5\0\0\a-i\3\4\b-ci\b-sr\nshfmt\14goimports\vstylua\rfourmolu\frustfmt\reslint_d\14prettierd\14filetypes\1\0\0\1\b\0\0\ttext\ash\bzsh\ttoml\tmake\tconf\ttmux\twith\20trim_whitespace\nsetup\18LspFormatting\24nvim_create_augroup\bapi\bvim\16diagnostics\15formatting\rbuiltins\fnull-ls\frequire\23€€À™\4\0", "config", "null-ls.nvim")
time([[Config for null-ls.nvim]], false)
-- Config for: nvim-treesitter
time([[Config for nvim-treesitter]], true)
try_loadstring("\27LJ\2\nî\2\0\0\5\0\14\0\0206\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\0025\1\a\0005\2\4\0005\3\5\0=\3\6\2=\2\b\1=\1\3\0006\1\0\0'\3\t\0B\1\2\0029\1\n\0015\3\f\0005\4\v\0=\4\r\3B\1\2\1K\0\1\0\21ensure_installed\1\0\0\1\t\0\0\tnorg\fhaskell\bcpp\6c\15javascript\trust\15typescript\ago\nsetup\28nvim-treesitter.configs\17install_info\1\0\0\nfiles\1\3\0\0\17src/parser.c\19src/scanner.cc\1\0\2\burl3https://github.com/nvim-neorg/tree-sitter-norg\vbranch\tmain\tnorg\23get_parser_configs\28nvim-treesitter.parsers\frequire\0", "config", "nvim-treesitter")
time([[Config for nvim-treesitter]], false)

-- Command lazy-loads
time([[Defining lazy-load commands]], true)
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Dispatch lua require("packer.load")({'vim-dispatch'}, { cmd = "Dispatch", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Make lua require("packer.load")({'vim-dispatch'}, { cmd = "Make", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Focus lua require("packer.load")({'vim-dispatch'}, { cmd = "Focus", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file Start lua require("packer.load")({'vim-dispatch'}, { cmd = "Start", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
time([[Defining lazy-load commands]], false)


_G._packer.inside_compile = false
if _G._packer.needs_bufread == true then
  vim.cmd("doautocmd BufRead")
end
_G._packer.needs_bufread = false

if should_profile then save_profiles() end

end)

if not no_errors then
  error_msg = error_msg:gsub('"', '\\"')
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
