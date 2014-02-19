" NeoBundle Setup {{{1
let g:bundles_path = '~/.vim/bundle/'

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

" tpope plugins {{{2
NeoBundle 'tpope/vim-haml'
NeoBundle 'tpope/vim-rake'
NeoBundle 'tpope/vim-rails'
NeoBundle 'tpope/vim-rbenv'
NeoBundle 'tpope/vim-eunuch'
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
NeoBundle 'tpope/vim-scriptease'
NeoBundle 'tpope/vim-unimpaired'
NeoBundle 'tpope/vim-speeddating'
NeoBundle 'tpope/vim-characterize'

" Other Plugins {{{2
NeoBundle 'sjl/gundo.vim'
NeoBundle 'kana/vim-vspec'
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'mattn/gist-vim', {'depends': 'mattn/webapi-vim'}
NeoBundle 'gregsexton/gitv'
NeoBundle 'ap/vim-css-color'
NeoBundle 'chrisbra/NrrwRgn'
NeoBundle 'godlygeek/tabular'
" NeoBundle 'FredKSchott/CoVim'
NeoBundle 'wavded/vim-stylus'
NeoBundle 'vim-ruby/vim-ruby'
NeoBundle 'honza/vim-snippets'
NeoBundle 'jpalardy/vim-slime'
NeoBundle 'garbas/vim-snipmate', {'depends': ['MarcWeber/vim-addon-mw-utils', 'tomtom/tlib_vim']}
" NeoBundle 'MarcWeber/ultisnips'
NeoBundle 'groenewege/vim-less'
NeoBundle 'jnwhiteh/vim-golang'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'b4winckler/vim-angry'
NeoBundle 'bkad/CamelCaseMotion'
NeoBundle 'derekwyatt/vim-scala'
NeoBundle 'digitaltoad/vim-jade'
NeoBundle 'marijnh/tern_for_vim', {
      \ 'build' : {
      \   'unix': 'npm install'
      \  }
      \ }
NeoBundle 'scrooloose/syntastic'
NeoBundle 'slim-template/vim-slim'
NeoBundle 'pangloss/vim-javascript'
NeoBundle 'kchmck/vim-coffee-script'
NeoBundle 'AndrewRadev/splitjoin.vim'
NeoBundle 'astashov/vim-ruby-debugger'
NeoBundle 'guns/xterm-color-table.vim'
NeoBundle 'Keithbsmiley/investigate.vim'
NeoBundle 'nelstrom/vim-textobj-rubyblock', {'depends': 'kana/vim-textobj-user'}

" My plugins {{{2
NeoBundleLocal ~/code/vim_plugins

" NeoBundleCheck {{{2
NeoBundleCheck
