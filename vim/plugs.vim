" Vim-Plug Setup {{{1
let g:vim_path = '~/.vim/'
let g:bundles_path = g:vim_path . 'bundle/'

if !filereadable(expand(g:vim_path.'autoload/plug.vim'))
  echo "Installing Vim-Plug\n"
  " silent execute '!git clone https://github.com/junegunn/vim-plug' g:bundles_path.'vim-plug'
  silent! execute '!curl -fLo' g:vim_path.'autoload/plug.vim https://raw.github.com/junegunn/vim-plug/master/plug.vim'
endif

" Plugs {{{1
" begin {{{2
call plug#begin(g:bundles_path)

" tpope plugins {{{3
Plug 'tpope/vim-rake'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-sensible'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fireplace'
Plug 'tpope/vim-leiningen'
Plug 'tpope/vim-obsession'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-scriptease'
Plug 'tpope/vim-unimpaired'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-rbenv'
Plug 'tpope/vim-projectionist'

Plug 'tpope/vim-haml', {'for': 'haml'}
Plug 'tpope/vim-rails', {'for': 'ruby'}
Plug 'tpope/vim-eunuch', {
      \ 'on': [
      \   'Unlink',
      \   'Remove',
      \   'Move',
      \   'Rename',
      \   'Chmod',
      \   'Mkdir',
      \   'Find',
      \   'Locate',
      \   'SudoEdit',
      \   'SudoWrite',
      \   'W'
      \ ]
      \}
Plug 'tpope/vim-abolish', {'on': ['Abolish', 'Subvert']}
Plug 'tpope/vim-bundler', {'for': 'ruby'}
Plug 'tpope/vim-cucumber', {'for': 'cucumber'}
Plug 'tpope/vim-dispatch', {'on': ['Make', 'Start', 'Copen', 'Dispatch', 'FocusDispatch']}
Plug 'tpope/vim-classpath', {'for': ['java', 'clojure']}

" Other plugins {{{3
Plug 'joonty/vdebug'
Plug 'kana/vim-vspec'
Plug 'kien/ctrlp.vim'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'jpalardy/vim-slime'
Plug 'digitaltoad/vim-jade'
Plug 'scrooloose/syntastic'
Plug 'guns/vim-clojure-static'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'mxw/vim-jsx'
Plug 'slim-template/vim-slim'


Plug 'mattn/webapi-vim'
Plug 'mattn/gist-vim', {'on': 'Gist'}

Plug 'sjl/gundo.vim', {'on': 'GundoToggle'}
Plug 'gregsexton/gitv', {'on': 'Gitv'}
Plug 'godlygeek/tabular', {'on': 'Tabular'}
Plug 'groenewege/vim-less', {'for': 'less'}
Plug 'jnwhiteh/vim-golang', {'for': 'go'}
Plug 'thinca/vim-quickrun', {'on': ['QuickRun', '<Plug>(quickrun}', '<Plug>(quickrun-op)']}
Plug 'derekwyatt/vim-scala', {'for': 'scala'}
" Plug 'marijnh/tern_for_vim', {'do': 'npm install', 'for': 'javascript'}
Plug 'pangloss/vim-javascript', {'for': 'javascript'}
Plug 'guns/xterm-color-table.vim', {'on': 'XtermColorTable'}
Plug 'PeterRincker/vim-argumentative'

let g:local_bundles = '~/code/vim_plugins/*'
for bundle in glob(g:local_bundles, 1, 1)
  if isdirectory(bundle)
    execute 'set runtimepath+='.bundle
  endif
endfor

call plug#end()
