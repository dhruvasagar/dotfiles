" Use repeat operator with visual selection {{{1
xnoremap . :normal! .<CR>

" To make Y inline with other capitals {{{1
nnoremap Y y$

" Q to :q<CR> a window {{{1
nmap <expr> <silent> Q empty(maparg('q', 'n')) ? ':q<CR>' : 'q'

" Focus on current buffer {{{1
nnoremap <C-W>O :tab split<CR>

" Dispatch {{{1
nnoremap <Leader>D :Dispatch<CR>

" Fugitive {{{1
nnoremap gsl :Glog<CR>
nnoremap gsd :Gdiff<CR>
nnoremap gse :Gedit<CR>
nnoremap gsb :Gblame<CR>
nnoremap gsw :Gwrite<CR>
nnoremap gsC :Gcommit<CR>
nnoremap gst :Gstatus<CR>
nnoremap gscd :Gcd<Bar>pwd<CR>
nnoremap gsld :Glcd<Bar>pwd<CR>

" Gundo {{{1
nnoremap U :GundoToggle<CR>

" Tabular {{{1
xnoremap z// :Tabular/
xnoremap z/= :Tabular/=<CR>
xnoremap z/: :Tabular/:<CR>

" Search {{{1
function! s:Vword()
  return getline('.')[col("'<")-1:col("'>")-1]
endfunction

xnoremap <silent> * <Esc>/\v<<C-R>=<SID>Vword()<CR>><CR>
xnoremap <silent> g* <Esc>/\v<C-R>=<SID>Vword()<CR><CR>

xnoremap <silent> # o<Esc>?\v<<C-R>=<SID>Vword()<CR>><CR>
xnoremap <silent> g# o<Esc>?\v<C-R>=<SID>Vword()<CR><CR>

nnoremap <silent> g// :grep -w <cword> <C-R>=getcwd()<CR><CR>
nnoremap <silent> g/* :grep <cword> <C-R>=getcwd()<CR><CR>

xnoremap <silent> g// :<C-U>grep -w <C-R>=<SID>Vword()<CR> <C-R>=getcwd()<CR><CR>
xnoremap <silent> g/* :<C-U>grep <C-R>=<SID>Vword()<CR> <C-R>=getcwd()<CR><CR>

" Copy current file with line number {{{1
command! -range CopyFileNameWithLineNumber <line1>,<line2>call functions#CopyFileNameWithLineNumber()
noremap y. :CopyFileNameWithLineNumber<CR>

" Hard Mode {{{1
map <Up> <Nop>
map <Down> <Nop>
map <Left> <Nop>
map <Right> <Nop>

imap <Up> <Nop>
imap <Down> <Nop>
imap <Left> <Nop>
imap <Right> <Nop>

" Use ` when ' {{{1
nnoremap ' `

" Copy to clipboard {{{1
nnoremap <silent> cy :set opfunc=functions#YankToClipboard<CR>g@
xnoremap <silent> cy :<C-U>call functions#YankToClipboard(visualmode(),1)<CR>
nnoremap <silent> cyy "+yy
