function! s:YankToClipboard(type, ...)
  let sel_save = &selection
  let &selection = "inclusive"
  let reg_save = @@
  if a:0
    silent exe "normal! gvy"
  elseif a:type == 'line'
    silent exe "normal! '[V']y"
  else
    silent exe "normal! `[v`]y"
  endif
  let @+=@@
  let &selection = sel_save
  let @@ = reg_save
endfunction

nnoremap <silent> cy :set opfunc=<SID>YankToClipboard<CR>g@
xnoremap <silent> cy :<C-U>call <SID>YankToClipboard(visualmode(),1)<CR>
nnoremap <silent> cyy "+yy
nnoremap <silent> cp "+p
