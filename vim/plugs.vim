" Vim-Plug Setup {{{1
let g:bundles_path = g:vim_home . 'plugged/'

if !filereadable(expand(g:vim_home.'autoload/plug.vim'))
  echo "Installing Vim-Plug\n"
  " silent execute '!git clone https://github.com/junegunn/vim-plug' g:bundles_path.'vim-plug'
  silent! execute '!curl -fLo' g:vim_home.'autoload/plug.vim https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
endif

" Plugs {{{1
call plug#begin(g:bundles_path)
" tpope plugins {{{2
Plug 'tpope/vim-rsi'
Plug 'tpope/vim-rake'
Plug 'tpope/vim-rbenv'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-endwise'
Plug 'tpope/vim-vinegar'
Plug 'tpope/vim-dispatch'
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
Plug 'tpope/vim-projectionist'
Plug 'tpope/vim-characterize'
" Plug 'tpope/vim-flagship'

Plug 'tpope/vim-haml', {'for': 'haml'}
Plug 'tpope/vim-rails', {'for': 'ruby'}
Plug 'tpope/vim-eunuch', {
      \ 'on': [
      \   'Unlink', 'Remove', 'Move', 'Rename', 'Chmod', 'Mkdir',
      \   'Find', 'Locate', 'SudoEdit', 'SudoWrite', 'W'
      \ ]
      \}
Plug 'tpope/vim-bundler', {'for': 'ruby'}
Plug 'tpope/vim-cucumber', {'for': 'cucumber'}
Plug 'tpope/vim-classpath', {'for': ['java', 'clojure']}

" Other plugins {{{2
Plug 'mxw/vim-jsx'
Plug 'joonty/vdebug'
Plug 'kana/vim-vspec'
Plug 'benmills/vimux'
Plug 'vimwiki/vimwiki'
Plug 'SirVer/ultisnips'
Plug 'janko-m/vim-test'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'honza/vim-snippets'
Plug 'jpalardy/vim-slime'
Plug 'avdgaag/vim-phoenix'
Plug 'digitaltoad/vim-jade'
Plug 'scrooloose/syntastic'
Plug 'tommcdo/vim-exchange'
Plug 'elixir-lang/vim-elixir'
Plug 'AndrewRadev/switch.vim'
Plug 'slim-template/vim-slim'
Plug 'guns/vim-clojure-static'
Plug 'kchmck/vim-coffee-script'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'PeterRincker/vim-argumentative'
Plug 'vim-scripts/ReplaceWithRegister'

Plug 'mattn/webapi-vim'
Plug 'mattn/gist-vim', {'on': 'Gist'}

Plug 'sjl/gundo.vim', {'on': 'GundoToggle'}
Plug 'gregsexton/gitv', {'on': 'Gitv'}
Plug 'godlygeek/tabular', {'on': 'Tabular'}
Plug 'jnwhiteh/vim-golang', {'for': 'go'}
Plug 'thinca/vim-quickrun', {'on': ['QuickRun', '<Plug>(quickrun)', '<Plug>(quickrun-op)']}
Plug 'derekwyatt/vim-scala', {'for': 'scala'}
Plug 'pangloss/vim-javascript', {'for': 'javascript'}
Plug 'guns/xterm-color-table.vim', {'on': 'XtermColorTable'}

" NeoVim plugins {{{2
if has('nvim')
  Plug 'kassio/neoterm'
endif

" My plugins {{{2
let g:my_plugins_path = '~/code/vim_plugins/'
Plug g:my_plugins_path . 'vim-less'
Plug g:my_plugins_path . 'vim-dotoo'
Plug g:my_plugins_path . 'vim-testify'
Plug g:my_plugins_path . 'vim-open-url'
Plug g:my_plugins_path . 'vim-prosession'
Plug g:my_plugins_path . 'vim-table-mode'
Plug g:my_plugins_path . 'vim-buffer-history'
Plug g:my_plugins_path . 'vim-railscasts-theme'
" }}}2
call plug#end()
