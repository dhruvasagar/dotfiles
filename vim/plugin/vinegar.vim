finish
if exists('g:loaded_vinegar')
  finish
endif
let g:loaded_vinegar = 1

function! s:vinegar()
  let fname = expand('%:t')
  edit %:p:h
  normal! gg
  call search('\<'.fname.'\>')
endfunction
nnoremap - :<C-U>call <SID>vinegar()<CR>
