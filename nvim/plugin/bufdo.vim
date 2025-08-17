function! s:BufDo(command)
  let curbuff = bufnr('%')
  execute 'bufdo' a:command
  execute 'buffer' curbuff
endfunction

command! -bar -nargs=+ -complete=command Bufdo call s:BufDo(<q-args>)
