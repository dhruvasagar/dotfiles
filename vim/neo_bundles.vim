" NeoBundle Setup {{{1
let g:bundles_path = '~/.vim/bundles/'

if has('vim_starting')
  execute 'set runtimepath+=' . g:bundles_path . 'neobundle.vim/'
  if !isdirectory(expand(g:bundles_path . 'neobundle.vim'))
    echo "Installing NeoBundle\n"
    silent execute '!mkdir -p ' . g:bundles_path
    silent execute '!git clone https://github.com/Shougo/neobundle.vim ' . g:bundles_path . 'neobundle.vim'
  endif
endif
call neobundle#rc(expand(g:bundles_path))

" NeoBundles {{{1
" Settings {{{2
let g:neobundle#types#git#default_protocol='ssh'

" Shougo Plugins {{{2
NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/vimproc.vim', {
      \ 'build' : {
      \     'windows' : 'make -f make_mingw32.mak',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak',
      \    },
      \ }
NeoBundle 'Shougo/vimshell.vim'
NeoBundle 'h1mesuke/unite-outline'

" tpope plugins {{{2
NeoBundle 'tpope/vim-haml'
NeoBundle 'tpope/vim-rake'
NeoBundle 'tpope/vim-rails'
NeoBundle 'tpope/vim-rbenv'
NeoBundle 'tpope/vim-ragtag'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-bundler'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'tpope/vim-cucumber'
NeoBundle 'tpope/vim-dispatch'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-markdown'
NeoBundle 'tpope/vim-sensible'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-obsession'
NeoBundle 'tpope/vim-commentary'
NeoBundle 'tpope/vim-scriptease'
NeoBundle 'tpope/vim-unimpaired'
NeoBundle 'tpope/vim-speeddating'
NeoBundle 'tpope/vim-characterize'

" github.com/vim-scripts plugins {{{2
NeoBundle 'L9'
NeoBundle 'argtextobj.vim'

" Other Plugins {{{2

NeoBundle 'sjl/gundo.vim'
NeoBundle 'kana/vim-vspec'
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'mattn/gist-vim', {'depends': 'mattn/webapi-vim'}
NeoBundle 'gregsexton/gitv'
NeoBundle 'sontek/rope-vim'
NeoBundle 'ap/vim-css-color'
NeoBundle 'chrisbra/NrrwRgn'
NeoBundle 'kana/vim-scratch'
NeoBundle 'majutsushi/tagbar'
NeoBundle 'godlygeek/tabular'
" NeoBundle 'FredKSchott/CoVim'
NeoBundle 'wavded/vim-stylus'
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'honza/vim-snippets'
NeoBundle 'jimenezrick/vimerl'
NeoBundle 'jpalardy/vim-slime'
NeoBundle 'lilydjwg/colorizer'
NeoBundle 'mattn/calendar-vim'
NeoBundle 'groenewege/vim-less'
NeoBundle 'jnwhiteh/vim-golang'
NeoBundle 'kablamo/vim-git-log'
NeoBundle 'MarcWeber/ultisnips'
NeoBundle 'osyo-manga/vim-over'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'bkad/CamelCaseMotion'
NeoBundle 'briancollins/vim-jst', {'depends': 'pangloss/vim-javascript'}
NeoBundle 'derekwyatt/vim-scala'
NeoBundle 'digitaltoad/vim-jade'
NeoBundle 'marijnh/tern_for_vim', {
      \ 'build' : {
      \   'unix': 'npm install'
      \  }
      \ }
NeoBundle 'scrooloose/syntastic'
NeoBundle 'elixir-lang/vim-elixir'
NeoBundle 'lukaszb/vim-web-indent'
NeoBundle 'slim-template/vim-slim'
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'hallison/vim-ruby-sinatra'
NeoBundle 'astashov/vim-ruby-debugger'
NeoBundle 'guns/xterm-color-table.vim'
NeoBundle 'Keithbsmiley/investigate.vim'
NeoBundle 'nelstrom/vim-textobj-rubyblock', {'depends': 'kana/vim-textobj-user'}
NeoBundle 'othree/javascript-libraries-syntax.vim'

" My plugins {{{2
NeoBundle 'dhruvasagar/vim-pipe'
NeoBundle 'dhruvasagar/vim-table-mode'
NeoBundle 'dhruvasagar/vim-railscasts-theme'

" NeoBundleCheck {{{2
NeoBundleCheck
