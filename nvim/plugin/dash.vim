if exists('g:loaded_dash')
  finish
endif
let g:loaded_dash = 1

function! s:Dash(search)
  exec 'silent !open "dash://' . join(split(a:search), ':') . '"'
  redraw!
endfunction

command! -bar -nargs=+ Dash call s:Dash(<q-args>)
nnoremap dx :Dash <C-R>=&filetype<CR> <cword><CR>
