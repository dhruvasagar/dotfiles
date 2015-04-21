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
