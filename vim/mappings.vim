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
if has('nvim')
  nnoremap stl :call neoterm#test#rerun()<CR>
  nnoremap stf :call neoterm#test#run('file')<CR>
  nnoremap sta :call neoterm#test#run('all')<CR>
  nnoremap stt :call neoterm#test#run('current')<CR>
  nnoremap stc :Ttoggle<CR>
else
  nnoremap stl :TestLast<CR>
  nnoremap stf :TestFile<CR>
  nnoremap sta :TestSuite<CR>
  nnoremap stu :TestNearest<CR>
endif


" Remap Esc in terminal for NeoVim
if has('nvim') && exists(':tnoremap')
  tnoremap <Esc> <C-\><C-n>
endif

nnoremap g> <ESC>vap:Twrite bottom-right<CR>
xnoremap g> :Twrite bottom-right<CR>

function! s:vinegar()
  let fname = expand('%:t')
  edit %:h
  normal! gg
  call search('\<'.fname.'\>')
endfunction
nnoremap - :<C-U>call <SID>vinegar()<CR>
