let g:underheadline_char = '-'

function! s:UnderHeadline()
  let cur = [line('.'), col('.')]
  exec 'normal! yypwv$r' . g:underheadline_char
  call cursor(cur)
endfunction

command! UnderHeadline call s:UnderHeadline()
nnoremap <Leader>uh :UnderHeadline<CR>
