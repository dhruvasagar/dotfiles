" Title String for tabs {{{1
function! functions#SetTitleString()
  set titlestring=%f\ %m
  set titlestring+=\ -\ [%{split(substitute(getcwd(),$HOME,'~',''),'/')[-1]}]
endfunction


" Fugitive {{{1
function! functions#GitExecInPath(cmd, ...) "{{{2
  if a:0
    let path = a:1
  elseif exists('b:git_dir')
    let path = b:git_dir
    let path = fnamemodify(path, ':h')
  else
    let path = fugitive#extract_git_dir('.')
    let path = fnamemodify(path, ':h')
  endif

  return system('cd ' . fnameescape(path) . '; ' . a:cmd)
endfunction

