" NeoBundle Setup {{{1
let g:bundles_path = '~/.vim/bundle/'

if has('vim_starting')
  execute 'set runtimepath+='.g:bundles_path.'neobundle.vim/'
  if !isdirectory(expand(g:bundles_path.'neobundle.vim'))
    echo "Installing NeoBundle\n"
    silent execute '!mkdir -p' g:bundles_path
    silent execute '!git clone https://github.com/Shougo/neobundle.vim' g:bundles_path.'neobundle.vim'
  endif
endif

" NeoBundles {{{1
" begin {{{2
call neobundle#begin(expand(g:bundles_path))
" Settings {{{3
let g:neobundle#types#git#default_protocol='ssh'

" Shougo plugins {{{3
NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundle 'Shougo/vimproc.vim', {
      \ 'build': {
      \   'windows' : 'make -f make_mingw32.mak',
      \   'cygwin' : 'make -f make_cygwin.mak',
      \   'mac' : 'make -f make_mac.mak',
      \   'unix' : 'make -f make_unix.mak'
      \ }
      \}
NeoBundle 'Shougo/unite.vim'

" tpope plugins {{{3
" NeoBundle 'tpope/timl'
NeoBundle 'tpope/vim-rake'
NeoBundle 'tpope/vim-repeat'
NeoBundle 'tpope/vim-endwise'
NeoBundle 'tpope/vim-vinegar'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'tpope/vim-sensible'
NeoBundle 'tpope/vim-surround'
NeoBundle 'tpope/vim-fireplace'
NeoBundle 'tpope/vim-leiningen'
NeoBundle 'tpope/vim-obsession'
NeoBundle 'tpope/vim-commentary'
NeoBundle 'tpope/vim-scriptease'
NeoBundle 'tpope/vim-unimpaired'
NeoBundle 'tpope/vim-speeddating'
NeoBundle 'tpope/vim-rbenv'
NeoBundle 'tpope/vim-projectionist'

NeoBundleLazy 'tpope/vim-haml', {
      \ 'autoload': {
      \   'filetypes': 'haml'
      \ }
      \}
NeoBundleLazy 'tpope/vim-rails', {
      \ 'autoload': {
      \   'filetypes': 'ruby'
      \ }
      \}
NeoBundleLazy 'tpope/vim-eunuch', {
      \ 'autoload': {
      \   'commands': [
      \     'Unlink',
      \     'Remove',
      \     'Move',
      \     'Rename',
      \     'Chmod',
      \     'Mkdir',
      \     'Find',
      \     'Locate',
      \     'SudoEdit',
      \     'SudoWrite',
      \     'W'
      \   ]
      \ }
      \}
NeoBundleLazy 'tpope/vim-abolish', {
      \ 'autoload': {
      \   'commands': [
      \     'Abolish',
      \     'Subvert'
      \   ],
      \   'mappings': [
      \     ['n', 'cr']
      \   ]
      \ }
      \}
NeoBundleLazy 'tpope/vim-bundler', {
      \ 'autoload': {
      \   'filetypes': 'ruby'
      \ }
      \}
NeoBundleLazy 'tpope/vim-cucumber', {
      \ 'autoload': {
      \   'filetypes': 'cucumber'
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
      \   'filetypes': ['java', 'clojure']
      \ }
      \}
NeoBundleLazy 'tpope/vim-characterize', {
      \ 'autoload': {
      \   'mappings': [['n', 'ga']]
      \ }
      \}

" Other Plugins {{{3
NeoBundle 'joonty/vdebug'
NeoBundle 'kana/vim-vspec'
NeoBundle 'kien/ctrlp.vim'
NeoBundle 'SirVer/ultisnips'
NeoBundle 'honza/vim-snippets'
NeoBundle 'jpalardy/vim-slime'
NeoBundle 'digitaltoad/vim-jade'
NeoBundle 'scrooloose/syntastic'
NeoBundle 'guns/vim-clojure-static'
NeoBundle 'AndrewRadev/splitjoin.vim'
NeoBundle 'mxw/vim-jsx'
NeoBundle 'slim-template/vim-slim'
NeoBundle 'thinca/vim-themis'

NeoBundleLazy 'sjl/gundo.vim', {
      \ 'autoload': {
      \   'commands': 'GundoToggle'
      \ }
      \}
NeoBundleLazy 'mattn/gist-vim', {
      \ 'depends': 'mattn/webapi-vim',
      \ 'autoload': {
      \   'commands': 'Gist'
      \ }
      \}
NeoBundleLazy 'gregsexton/gitv', {
      \ 'autoload': {
      \   'commands': 'Gitv'
      \ }
      \}
NeoBundleLazy 'godlygeek/tabular', {
      \ 'autoload': {
      \   'commands': 'Tabular'
      \ }
      \}
NeoBundleLazy 'groenewege/vim-less', {
      \ 'autoload': {
      \   'filetypes': 'less'
      \ }
      \}
NeoBundleLazy 'jnwhiteh/vim-golang', {
      \ 'autoload': {
      \   'filetypes': 'go'
      \ }
      \}
NeoBundleLazy 'thinca/vim-quickrun', {
      \ 'autoload': {
      \   'commands': 'QuickRun',
      \   'mappings': [['n', '<Plug>(quickrun)'], ['o', '<Plug>(quickrun-op)']]
      \ }
      \}
NeoBundleLazy 'derekwyatt/vim-scala', {
      \ 'autoload': {
      \   'filetypes': 'scala'
      \ }
      \}
NeoBundleLazy 'marijnh/tern_for_vim', {
      \ 'build' : {
      \   'unix': 'npm install'
      \  },
      \ 'autoload': {
      \   'filetypes': 'javascript'
      \ }
      \}
NeoBundleLazy 'pangloss/vim-javascript', {
      \ 'autoload': {
      \   'filetypes': 'javascript'
      \ }
      \}
NeoBundleLazy 'guns/xterm-color-table.vim', {
      \ 'autoload': {
      \   'commands': 'XtermColorTable'
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

" My plugins {{{3
NeoBundleLocal ~/code/vim_plugins

" end {{{2
call neobundle#end()

" NeoBundleCheck {{{2
NeoBundleCheck
