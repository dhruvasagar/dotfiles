let g:underheadline_char = '-'

function! s:UnderHeadLine()
  let cur = [line('.'), col('.')]
  exec 'normal! yypwv$r' . g:underheadline_char
  call cursor(cur)
endfunction

command! UnderHeadline call s:UnderHeadLine()
nnoremap <Leader>uh :UnderHeadLine<CR>
