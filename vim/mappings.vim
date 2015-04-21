" Use repeat operator with visual selection {{{1
xnoremap . :normal! .<CR>

" To make Y inline with other capitals {{{1
nnoremap Y y$

" Q to :q<CR> a window {{{1
nmap <expr> <silent> Q empty(maparg('q', 'n')) ? ':q<CR>' : 'q'

" Focus on current buffer {{{1
nnoremap <C-W>t :tab split<CR>

" Dispatch {{{1
nnoremap d<Enter> :Dispatch<CR>

" Gundo {{{1
nnoremap U :GundoToggle<CR>

" Tabular {{{1
xnoremap <expr> z/ ':Tabular/'.nr2char(getchar()).'<CR>'

" Use ` when ' {{{1
nnoremap ' `

" Remap Esc in terminal for NeoVim
if exists(':tnoremap')
  tnoremap <Esc> <C-\><C-n>
endif
