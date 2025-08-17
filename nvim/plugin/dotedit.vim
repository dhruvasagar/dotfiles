function! s:DotComplete(ArgLead, CmdLine, CursorPos) abort
  let paths = globpath(expand('~/dotfiles'), '**/'.a:ArgLead.'*', 1, 1)
  for ppath in ['packup', 'backup']
    let paths = filter(paths, 'v:val !~# "'.ppath.'"')
  endfor
  return paths
endfunction

function! s:DotEdit(cmd, fname) abort
  exec a:cmd a:fname
endfunction

command! -bar -nargs=1 -complete=customlist,s:DotComplete Dedit call s:DotEdit('edit', <q-args>)
command! -bar -nargs=1 -complete=customlist,s:DotComplete Dsplit call s:DotEdit('split', <q-args>)
command! -bar -nargs=1 -complete=customlist,s:DotComplete Dvsplit call s:DotEdit('vsplit', <q-args>)
command! -bar -nargs=1 -complete=customlist,s:DotComplete Dtabedit call s:DotEdit('tabedit', <q-args>)
