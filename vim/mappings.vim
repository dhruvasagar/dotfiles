" List of available normal mode mappings for future use :
"
" * cq
" * cu
" * cd
" * cm
" * c<CR>
"
" * dq
" * dr
" * dy
" * du
" * dx
" * dc
" * dm
"
" * gy
" * go
" * gz
"
" * yq
" * yr
" * yu
" * yx
" * yc
" * ym
"
" * zq
" * zy
" * zp

" Use repeat operator with visual selection {{{1
xnoremap . :normal! .<CR>

" To make Y inline with other capitals {{{1
nnoremap Y y$

" Q to :q<CR> a window {{{1
nmap <expr> <silent> Q empty(maparg('q', 'n')) ? ':q<CR>' : 'q'

" Focus on current buffer {{{1
nnoremap <C-W>m :wincmd _<Bar>wincmd <Bar><CR>

" Dispatch {{{1
nnoremap d<CR> :Dispatch<CR>

" Gundo {{{1
nnoremap U :GundoToggle<CR>

" Tabular {{{1
nnoremap <expr> z/ ':Tabular/'.nr2char(getchar()).'<CR>'
xnoremap <expr> z/ ':Tabular/'.nr2char(getchar()).'<CR>'

" Use ` when ' {{{1
nnoremap ' `

" Test {{{1
nnoremap cil :TestLast<CR>
nnoremap cif :TestFile<CR>
nnoremap cia :TestSuite<CR>
nnoremap cit :TestNearest<CR>
if has('nvim')
  nnoremap cic :Ttoggle<CR>
endif


" Remap Esc in terminal for NeoVim
if has('nvim') && exists(':tnoremap')
  tnoremap <Esc> <C-\><C-n>
endif

nnoremap g> <ESC>vap:Twrite bottom-right<CR>
xnoremap g> :Twrite bottom-right<CR>

function! s:vinegar()
  let fname = expand('%:t')
  edit %:p:h
  normal! gg
  call search('\<'.fname.'\>')
endfunction
nnoremap - :<C-U>call <SID>vinegar()<CR>

nnoremap <, :SidewaysLeft<CR>
nnoremap >, :SidewaysRight<CR>

nnoremap <silent> go :Goyo<CR>
