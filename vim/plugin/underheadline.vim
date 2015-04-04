if exists('g:loaded_underheadline')
  finish
endif
let g:loaded_underheadline = 1

let g:underheadline_char = '-'

function! s:UnderHeadLine()
  let cur = [line('.'), col('.')]
  exec 'normal! yypwv$r' . g:underheadline_char
  call cursor(cur)
endfunction

command! UnderHeadline call s:UnderHeadLine()

nnoremap <Plug>(underheadline) :call <SID>UnderHeadLine()<CR>

if !exists('g:underheadline_disable_mappings')
  nmap <Leader>uh <Plug>(underheadline)
endif
