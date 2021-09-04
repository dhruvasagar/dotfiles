function! s:Extract(bang,cmd,fname) range abort
  let extn = expand('%:e')
  if empty(extn)
    let l:fname = a:fname
  else
    let l:fname = a:fname . '.' . extn
  endif
  let [first, last] = [a:firstline, a:lastline]
  let range = first.",".last
  let spaces = matchstr(getline(first),"^ *")
  let buf = @@
  silent exe range."yank"
  let partial = @@
  let @@ = buf
  let old_ai = &ai
  try
    let &ai = 0
    silent exe "norm! :".range."change\<CR>".spaces.a:fname."\<CR>.\<CR>"
  finally
    let &ai = old_ai
  endtry
  execute a:cmd . ' ' . expand('%:p:h') . '/' . l:fname
  let @@ = partial
  silent put
  0delete
  let @@ = buf
  if !empty(spaces)
    silent! exe '%substitute/^' . spaces . '//'
  endif
endfunction

command! -bar -nargs=1 -range SExtract :<line1>,<line2>call s:Extract(<bang>0,"split",<q-args>)
command! -bar -nargs=1 -range VExtract :<line1>,<line2>call s:Extract(<bang>0,"vsplit",<q-args>)
command! -bar -nargs=1 -range TExtract :<line1>,<line2>call s:Extract(<bang>0,"tabnew",<q-args>)
