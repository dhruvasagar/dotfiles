function! s:Edit(cmd, fname)
  let fname_lnum = split(a:fname, ':')
  if len(fname_lnum) == 1
    exe a:cmd fname_lnum[0]
  else
    exe a:cmd '+'.fname_lnum[1] fname_lnum[0]
  endif
endfunction

command! -nargs=1 -complete=file Edit call s:Edit('edit', <q-args>)
command! -nargs=1 -complete=file Esplit call s:Edit('split', <q-args>)
command! -nargs=1 -complete=file Evsplit call s:Edit('vsplit', <q-args>)
command! -nargs=1 -complete=file Etabedit call s:Edit('tabe', <q-args>)
