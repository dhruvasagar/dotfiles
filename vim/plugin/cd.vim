function! s:CdComplete(ArgLead, CmdLine, CursorPos)
  let pattern = empty(a:ArgLead) ? '*/' : '*' . a:ArgLead . '*/'
  return map(globpath(&cdpath, pattern, 1, 1), 'fnamemodify(v:val, ":h:t")')
endfunction

command! -bar -nargs=1 -complete=customlist,s:CdComplete Cd cd <args>
