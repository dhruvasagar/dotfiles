function! s:Vword()
  return getline('.')[col("'<")-1:col("'>")-1]
endfunction

function! s:search(type, ...)
  let sel_save = &selection
  let &selection = 'inclusive'
  let reg_save = @@

  if a:0
    silent exe 'normal! gvy'
  elseif a:type == 'line'
    silent exe "normal! '[V']y"
  else
    silent exe "normal! `[v`]y"
  endif

  silent! exec ":grep '" . @@ . "'" getcwd()
  redraw!
  echo ":grep '" . @@ . "'" getcwd()

  let &selection = sel_save
  let @@ = reg_save
endfunction

nnoremap <silent> g/ :set opfunc=<SID>search<CR>g@
xnoremap <silent> g/ :<C-U>call <SID>search(visualmode(), 1)<CR>

xnoremap <silent> * <Esc>/\v<<C-R>=<SID>Vword()<CR>><CR>
xnoremap <silent> g* <Esc>/\v<C-R>=<SID>Vword()<CR><CR>

xnoremap <silent> # o<Esc>?\v<<C-R>=<SID>Vword()<CR>><CR>
xnoremap <silent> g# o<Esc>?\v<C-R>=<SID>Vword()<CR><CR>
