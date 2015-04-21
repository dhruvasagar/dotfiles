function! s:View(cmd)
  redir => output
  silent execute a:cmd
  redir END
  Ssplit
  call setline(1, split(output, '\n'))
endfunction

command! -bar -nargs=1 -complete=command View call s:View(<q-args>)
