local fn = vim.fn
local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', 'https://github.com/wbthomason/packer.nvim', install_path})
end

vim.cmd 'packadd packer.nvim'

return require('packer').startup(function()
  use 'wbthomason/packer.nvim'

  use 'tpope/vim-rsi'
  use 'tpope/vim-rake'
  use 'tpope/vim-tbone'
  use 'tpope/vim-apathy'
  use 'tpope/vim-repeat'
  use 'tpope/vim-abolish'
  use 'tpope/vim-endwise'
  use 'tpope/vim-rhubarb'
  use {
    'tpope/vim-dispatch',
    opt = true,
    cmd = {'Dispatch', 'Make', 'Focus', 'Start'}
  }
  use 'tpope/vim-fugitive'
  use 'tpope/vim-sensible'
  use { 'tpope/vim-fireplace', opt = true }
  use { 'tpope/vim-leiningen', opt = true }
  use 'tpope/vim-obsession'
  use 'tpope/vim-commentary'
  use 'tpope/vim-scriptease'

  use {
    'folke/lua-dev.nvim',
    config = function()
      local luadev = require('lua-dev').setup{}
      local lspconfig = require('lspconfig')
      -- lspconfig.sumneko_lua.setup(luadev)
      -- lspconfig[%YOUR_LSP_SERVER%].setup {
      --   capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
      -- }
    end,
    requires = {'neovim/nvim-lspconfig'}, -- {'hrsh7th/cmp-nvim-lsp'}},
  }
  use 'andymass/vim-matchup'
  use 'tpope/vim-unimpaired'
  use 'tpope/vim-speeddating'
  use 'tpope/vim-characterize'
  use { 'tpope/vim-sexp-mappings-for-regular-people', opt = true }

  use 'tpope/vim-rails'
  use 'tpope/vim-eunuch'
  use 'tpope/vim-bundler'
  use 'tpope/vim-cucumber'

  use 'wellle/targets.vim'
  use 'tommcdo/vim-exchange'
  use 'AndrewRadev/switch.vim'
  use 'AndrewRadev/sideways.vim'
  use 'AndrewRadev/splitjoin.vim'
  use 'simnalamburt/vim-mundo'
  use 'machakann/vim-sandwich'
  use 'rstacruz/vim-closer'

  use 'pangloss/vim-javascript'
  use 'MaxMEllon/vim-jsx-pretty'
  use 'burnettk/vim-angular'

  use { 'guns/vim-sexp', opt = true }
  use { 'guns/vim-clojure-static', opt = true }

  use 'neovimhaskell/haskell-vim'
  use 'kana/vim-vspec'
  use 'avdgaag/vim-phoenix'
  use 'digitaltoad/vim-jade'
  use 'elixir-lang/vim-elixir'
  use 'slim-template/vim-slim'
  use { 'fatih/vim-go', run = ':GoInstallBinaries' }

  use 'racer-rust/vim-racer'
  use {
    'simrat39/rust-tools.nvim',
    requires = {{'mfussenegger/nvim-dap'}, {'nvim-lua/plenary.nvim'}},
    config = function()
      require('rust-tools').setup({})
    end
  }
  use {
    "rcarriga/nvim-dap-ui",
    requires = {"mfussenegger/nvim-dap"}
  }
  use 'leafgarland/typescript-vim'
  use 'purescript-contrib/purescript-vim'
  use 'SirVer/ultisnips'
  use 'honza/vim-snippets'
  use 'benmills/vimux'
  use 'janko-m/vim-test'
  use 'dense-analysis/ale'
  use 'diepm/vim-rest-console'
  use 'powerman/vim-plugin-AnsiEsc'
  use {
    'mattn/gist-vim',
    requires = {{'mattn/webapi-vim'}}
  }
  use 'editorconfig/editorconfig-vim'
  use 'godlygeek/tabular'
  use 'vim-scripts/SyntaxRange'
  -- use { 'junegunn/fzf', run = function() vim.fn['fzf#install'](0) end }
  -- use 'junegunn/fzf.vim'
  use {
    'nvim-telescope/telescope.nvim',
    requires = {{'nvim-lua/plenary.nvim' }}
  }
  use {
    'nvim-treesitter/nvim-treesitter',
    config = function()
      local parser_configs = require('nvim-treesitter.parsers').get_parser_configs()
      parser_configs.norg = {
        install_info = {
          url = " https://github.com/nvim-neorg/tree-sitter-norg",
          files = { "src/parser.c", "src/scanner.cc" },
          branch = "main"
        },
      }
      require('nvim-treesitter.configs').setup {
        ensure_installed = {
          "norg",
          "haskell",
          "cpp",
          "c",
          "javascript",
          "rust",
          "typescript",
          "go",
        },
      }
    end,
    run = ':TSUpdate'
  }

  use { 'neoclide/coc.nvim', branch = 'release' }
  use 'neovim/nvim-lspconfig'
  use {
    'kabouzeid/nvim-lspinstall',
    config = function()
      local function setup_servers()
        require'lspinstall'.setup()
        local servers = require'lspinstall'.installed_servers()
        for _, server in pairs(servers) do
          require'lspconfig'[server].setup{}
        end
      end

      setup_servers()

      -- Automatically reload after `:LspInstall <server>` so we don't have to restart neovim
      require'lspinstall'.post_install_hook = function ()
        setup_servers() -- reload installed servers
        -- vim.cmd("bufdo e") -- this triggers the FileType autocmd that starts the server
      end
    end
  }
  -- use {
  --   'nvim-lua/completion-nvim',
  --   config = function()
  --     vim.cmd("autocmd BufEnter * lua require'completion'.on_attach()")
  --   end
  -- }
  -- use {
  --   'hrsh7th/nvim-cmp',
  --   config = function()
  --     local cmp = require('cmp')
  --     cmp.setup {
  --       snippet = {
  --         expand = function(args)
  --           vim.fn["UltiSnips#Anon"](args.body)
  --         end,
  --       },
  --       sources = {
  --         { name = 'nvim_lsp' },
  --         { name = 'ultisnips' },
  --         { name = 'buffer' },
  --       },
  --     }
  --   end
  -- }

  use {
    'ellisonleao/glow.nvim',
    run = 'GlowInstall'
  }
  use 'SidOfc/mkdx'
  use { 'kkoomen/vim-doge', run = function() vim.fn['doge#install'](0) end }
  use 'junegunn/goyo.vim'
  use 'vim-scripts/DrawIt'
  use 'junegunn/vim-emoji'
  -- use {
  --   'RRethy/vim-hexokinase',
  --   run = 'make hexokinase'
  -- }
  use 'norcalli/nvim-colorizer.lua'
  use 'guns/xterm-color-table.vim'
  use 'vimwiki/vimwiki'
  use 'ledger/vim-ledger'
  use 'puremourning/vimspector'
  -- use 'Yggdroot/indentLine'
  use 'towolf/vim-helm'
  use 'Lenovsky/nuake'
  use {
    'lukas-reineke/indent-blankline.nvim',
    config = function()
      require('indent_blankline').setup {
        buftype_exclude = {"terminal"}
      }
    end
  }
  use {
    'pwntester/octo.nvim',
    config = function()
      require('octo').setup()
    end
  }
  -- use {
  --   'marko-cerovac/material.nvim',
  --   config = function()
  --     vim.g.material_style = "deep ocean"
  --     -- vim.cmd('colorscheme material')
  --   end
  -- }
  use 'kyazdani42/nvim-web-devicons'
  use 'mustache/vim-mustache-handlebars'

  use {
    'nvim-neorg/neorg',
    config = function()
      require('neorg').setup {
        load = {
          ["core.defaults"] = {},
          ["core.keybinds"] = {
            config = {
              default_keybinds = true,
              neorg_leader = "<Leader>o"
            }
          },
          ["core.norg.concealer"] = {},
          ["core.integrations.telescope"] = {},
          ["core.norg.dirman"] = {
            config = {
              workspaces = {
                my_workspace = "~/Dropbox/Documents/neorg"
              }
            }
          },
        },
      }
    end,
    requires = {{'nvim-lua/plenary.nvim'}, {'nvim-neorg/neorg-telescope'}},
  }

  use '~/dotfiles/vim/pack/packup/start/vim-less'
  use '~/dotfiles/vim/pack/packup/start/vim-marp'
  use '~/dotfiles/vim/pack/packup/start/vim-zoom'
  use '~/dotfiles/vim/pack/packup/start/vim-dotoo'
  use '~/dotfiles/vim/pack/packup/start/vim-pairify'
  use '~/dotfiles/vim/pack/packup/start/vim-testify'
  use '~/dotfiles/vim/pack/packup/start/vim-open-url'
  use '~/dotfiles/vim/pack/packup/start/vim-table-mode'
  use '~/dotfiles/vim/pack/packup/start/vim-buffer-history'
  use {
    '~/dotfiles/vim/pack/packup/start/vim-railscasts-theme',
    config = function()
      vim.cmd('colorscheme railscasts')
    end
  }
  use '~/dotfiles/vim/pack/packup/start/vim-github-review'
  use '~/dotfiles/vim/pack/packup/start/vim-comp'
  use '~/dotfiles/vim/pack/packup/opt/vim-prosession'
end)
