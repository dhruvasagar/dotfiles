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

NeoBundleLazy 'Shougo/vimshell.vim', {
      \ 'depends': [['Shougo/vimproc.vim', {
      \   'build' : {
      \     'windows' : 'make -f make_mingw32.mak',
      \     'cygwin' : 'make -f make_cygwin.mak',
      \     'mac' : 'make -f make_mac.mak',
      \     'unix' : 'make -f make_unix.mak'
      \   }
      \ }]],
      \ 'autoload' : {
      \   'commands': [
      \     'VimShell',
      \     'VimShellExecute',
      \     'VimShellInteractive',
      \     'VimShellTerminal',
      \     'VimShellPop'
      \   ],
      \   'functions': ['vimshel#interactive#send']
      \ }
      \}

" tpope plugins {{{2
" NeoBundle 'tpope/timl'
NeoBundle 'tpope/vim-rake'
NeoBundle 'tpope/vim-eunuch'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-sleuth'
NeoBundle 'tpope/vim-abolish'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-sensible'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-obsession'
NeoBundle 'tpope/vim-commentary'
NeoBundle 'tpope/vim-scriptease'
NeoBundle 'tpope/vim-unimpaired'
NeoBundle 'tpope/vim-speeddating'
NeoBundle 'tpope/vim-characterize'

NeoBundleLazy 'tpope/vim-haml', {
      \ 'autoload': {
      \   'filetypes': ['haml']
      \ }
      \}
NeoBundleLazy 'tpope/vim-rails', {
      \ 'autoload': {
      \   'filetypes': ['ruby']
      \ }
      \}
NeoBundleLazy 'tpope/vim-rbenv', {
      \ 'autoload': {
      \   'filetypes': ['ruby']
      \ }
      \}
NeoBundleLazy 'tpope/vim-bundler', {
      \ 'autoload': {
      \   'filetypes': ['ruby']
      \ }
      \}
NeoBundleLazy 'tpope/vim-cucumber', {
      \ 'autoload': {
      \   'filetypes': ['cucumber']
      \ }
      \}
NeoBundleLazy 'tpope/vim-dispatch', {
      \ 'autoload': {
      \   'commands': [
      \     'Make',
      \     'Start',
      \     'Copen',
      \     'Dispatch',
      \     'FocusDispatch'
      \   ]
      \ }
      \}
NeoBundleLazy 'tpope/vim-classpath', {
			\ 'autoload': {
			\   'filetypes': ['java'],
			\ }
			\}

" Other Plugins {{{2
NeoBundle 'kana/vim-vspec'
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'gregsexton/gitv'
NeoBundle 'ap/vim-css-color'
NeoBundle 'chrisbra/NrrwRgn'
NeoBundle 'honza/vim-snippets'
" NeoBundle 'garbas/vim-snipmate', {'depends': ['MarcWeber/vim-addon-mw-utils', 'tomtom/tlib_vim']}
NeoBundle 'SirVer/ultisnips'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'bkad/CamelCaseMotion'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'AndrewRadev/splitjoin.vim'

NeoBundleLazy 'dahu/vimple', {
      \ 'autoload': {
      \   'commands': ['View']
      \ }
      \}
NeoBundleLazy 'sjl/gundo.vim', {
      \ 'autoload': {
      \   'commands': ['GundoToggle']
      \ }
      \}
NeoBundleLazy 'mattn/gist-vim', {
      \ 'depends': 'mattn/webapi-vim',
      \ 'autoload': {
      \   'commands': ['Gist']
      \ }
      \}
NeoBundleLazy 'godlygeek/tabular', {
      \ 'autoload': {
      \   'commands': ['Tabular']
      \ }
      \}
NeoBundleLazy 'wavded/vim-stylus', {
      \ 'autoload': {
      \   'filetypes': ['stylus']
      \ }
      \}
NeoBundleLazy 'groenewege/vim-less', {
      \ 'autoload': {
      \   'filetypes': ['less']
      \ }
      \}
NeoBundleLazy 'jnwhiteh/vim-golang', {
      \ 'autoload': {
      \   'filetypes': ['go']
      \ }
      \}
NeoBundleLazy 'thinca/vim-quickrun', {
      \ 'autoload': {
      \   'mappings': [['n', '<Plug>(quickrun)'], ['o', '<Plug>(quickrun-op)']]
      \ }
      \}
NeoBundleLazy 'derekwyatt/vim-scala', {
      \ 'autoload': {
      \   'filetypes': ['scala']
      \ }
      \}
NeoBundleLazy 'digitaltoad/vim-jade', {
      \ 'autoload': {
      \   'filetypes': ['jade']
      \ }
      \}
NeoBundleLazy 'marijnh/tern_for_vim', {
      \ 'build' : {
      \   'unix': 'npm install'
      \  },
      \ 'autoload': {
      \   'filetypes': ['javascript']
      \ }
      \}
NeoBundleLazy 'slim-template/vim-slim', {
      \ 'autoload': {
      \   'filetypes': ['slim']
      \ }
      \}
NeoBundleLazy 'pangloss/vim-javascript', {
      \ 'autoload': {
      \   'filetypes': ['javascript']
      \ }
      \}
NeoBundleLazy 'kchmck/vim-coffee-script', {
      \ 'autoload': {
      \   'filetypes': ['coffee']
      \ }
      \}
NeoBundleLazy 'astashov/vim-ruby-debugger', {
      \ 'autoload': {
      \   'filetypes': ['ruby']
      \ }
      \}
NeoBundleLazy 'guns/xterm-color-table.vim', {
      \ 'autoload': {
      \   'commands': ['XtermColorTable']
      \ }
      \}
NeoBundleLazy 'nelstrom/vim-textobj-rubyblock', {
      \ 'depends': 'kana/vim-textobj-user',
      \ 'autoload': {
      \   'filetypes': ['ruby']
      \ }
      \}
NeoBundleLazy 'PeterRincker/vim-argumentative', {
      \ 'autoload': {
      \   'mappings': [
      \    ['nxo', '[,', '],'],
      \    ['n', '<,', '>,'],
      \    ['xo', 'i,', 'a,']
      \   ]
      \ }
      \}

" Temporary Plugins {{{2

" My plugins {{{2
NeoBundleLocal ~/code/vim_plugins

" NeoBundleCheck {{{2
NeoBundleCheck
