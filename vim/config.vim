" Core Options {{{1

" Set encoding to utf-8, fixes macvim bug for
" using unicode characters in listchars
scriptencoding utf-8

let mapleader="\<Space>"

set autoread
set noswapfile

set confirm
set ignorecase
set infercase
set smartcase
set hlsearch
set splitbelow
set splitright

set exrc

" Backup {{{2
set backup backupdir=~/.vim/backup
if !isdirectory(expand(&backupdir))
  call mkdir(expand(&backupdir), 'p')
endif

" Vim supports dictionary autocomplete Ctrl_X+Ctrl_K
set dictionary+=/usr/share/dict/words
" Vim supports thesaurus autocomplete Ctrl_X+Ctrl_T
set thesaurus+=~/.vim/spell/mthesaur.txt

if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor\ --column
else
  set grepprg=grep\ -rnH\ --exclude=tags\ --exclude-dir=.git\ --exclude-dir=node_modules
endif

" Undo Options {{{2
set undofile                  " Use undofile to persist undo history
set undolevels=1000           " Increase undo level to 1000
set undodir=~/.vim/undo       " Specifies where to keep undo files
if !isdirectory(expand(&undodir))
  call mkdir(expand(&undodir), 'p')
endif

" Spell Settings {{{2
set spelllang=en_us
set spellsuggest=best,5

" Formatting Options {{{1
" set nowrap
set linebreak
set breakindent
set smarttab
set expandtab
set shiftwidth=2
set softtabstop=2
set nojoinspaces
set showmatch
set number
set numberwidth=3
set nostartofline
set fileformat=unix
set virtualedit=block
set formatoptions+=tcroqnjl21
set colorcolumn=80
set showtabline=2

" Visual Options {{{1
syntax on
filetype plugin indent on

set title
set hidden
set cmdheight=1
set matchtime=5
set visualbell
set noerrorbells
set ttymouse=xterm2
set lazyredraw
set cursorline
set updatetime=1000

set list
if &termencoding ==# 'utf-8' && &encoding ==# 'utf-8' && version >= 700
  set listchars=tab:›\ 
  set listchars+=eol:¬
  set listchars+=trail:⋅
  set listchars+=extends:›
  set listchars+=precedes:‹
  set listchars+=nbsp:+

  set fillchars=stl:\ 
  set fillchars+=stlnc:\ 
  set fillchars+=vert:\|
  set fillchars+=fold:\⋅
  set fillchars+=diff:-

  set showbreak=↪\ 
else
  set listchars=tab:\ \ 
  set listchars+=eol:$
  set listchars+=trail:~
  set listchars+=extends:>
  set listchars+=precedes:<
  set listchars+=nbsp:+

  set fillchars=stl:\ 
  set fillchars+=stlnc:\ 
  set fillchars+=vert:\|
  set fillchars+=fold:\-
  set fillchars+=diff:-

  set showbreak=->\ 
endif
set mousehide
set mouse=a                   " Enables mouse within terminals

set foldenable
set foldmethod=syntax
set foldlevelstart=1

if has('balloon_eval') && has('unix')
  set ballooneval
endif

" Provides nice wild menu completion, makes command completion in ambiguous
" case very easy
set wildcharm=<C-Z>
set wildmenu wildmode=list:longest,full
set wildignorecase
set wildignore+=*.swp,*.bak,*.pyc,*.class,*.o,*.obj,tags

" Making <S-Tab> work
" execute 'set t_kB=' . nr2char(27) . '[Z'
set t_kB=[Z

if exists('$TMUX')
  let &t_SI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=1\x7\<Esc>\\"
  let &t_EI = "\<Esc>Ptmux;\<Esc>\<Esc>]50;CursorShape=0\x7\<Esc>\\"
else
  let &t_SI = "\<Esc>]50;CursorShape=1\x7"
  let &t_EI = "\<Esc>]50;CursorShape=0\x7"
endif

" let g:solarized_termcolors=256
" color solarized
color railscasts
