function! functions#SetTitleString() "{{{1
  set titlestring=%f\ %m
  set titlestring+=\ -\ [%{split(substitute(getcwd(),$HOME,'~',''),'/')[-1]}]
endfunction


" Fugitive {{{1
function! s:GitShortRefNames(names, full_name) "{{{2
  if a:full_name
    let expr = 'v:val[11:]'
  else
    let expr = 'v:val[strridx(v:val, "/")+1:]'
  endif
  return map(a:names, expr)
endfunction

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

  return system('cd ' . path . '; ' . a:cmd)
endfunction

function! s:GitComplete(ArgLead, Cmdline, Cursor, ...) "{{{2
  let refs = 'refs/heads/'
  if a:0 == 1 && a:1 !=? 'branch'
    let refs = 'refs/' . a:1
    let full_name = 1
  elseif a:0 == 2 && a:1 ==? 'branch'
    let refs = refs . a:2
    let full_name = 0
  endif

  let cmd = 'git for-each-ref --format="%(refname)" ' . refs
  if !empty(a:ArgLead)
    let cmd = cmd . ' | sed "s/.*\/\(.*\)/\1/" | grep ^' . a:ArgLead . ' 2>/dev/null'
  endif
  return s:GitShortRefNames(split(functions#GitExecInPath(cmd)), full_name)
endfunction

function! s:GitExtraComplete(ArgLead, CmdLine, Cursor, type) "{{{2
  if (empty(a:ArgLead) || a:ArgLead =~? '^f\%[inish]$') && a:CmdLine !~? 'finish\s*$'
    return ['finish']
  else
    return s:GitComplete(a:ArgLead, a:CmdLine, a:Cursor, 'branch', a:type)
  endif
endfunction

function! functions#GitBugComplete(ArgLead, CmdLine, Cursor) "{{{2
  return s:GitExtraComplete(a:ArgLead, a:CmdLine, a:Cursor, 'bug')
endfunction

function! functions#GitFeatureComplete(ArgLead, CmdLine, Cursor) "{{{2
  return s:GitExtraComplete(a:ArgLead, a:CmdLine, a:Cursor, 'feature')
endfunction

function! functions#GitRefactorComplete(ArgLead, CmdLine, Cursor) "{{{2
  return s:GitExtraComplete(a:ArgLead, a:CmdLine, a:Cursor, 'refactor')
endfunction

" Random Number {{{1
function! functions#Rand()
  return str2nr(matchstr(reltimestr(reltime()), '\v\.@<=\d+')[1:])
endfunction

" Filter quickfix / location list {{{1
function! functions#FilterQuickfixList(bang, pattern)
  let [cmp, and_or] = a:bang ? ['!~#', '&&'] : ['=~#', '||']
  call setqflist(filter(getqflist(), "bufname(v:val.bufnr) " . cmp . " a:pattern " . and_or . " v:val.text " . cmp . " a:pattern"))
endfunction

function! functions#FilterLocationList(bang, pattern)
  let [cmp, and_or] = a:bang ? ['!~#', '&&'] : ['=~#', '||']
  call setloclist('%', filter(getloclist('%'), "bufname(v:val.bufnr) " . cmp . " a:pattern " . and_or . " v:val.text " . cmp . " a:pattern"))
endfunction

" Extract {{{1
function! functions#Extract(bang,cmd,fname) range abort
  let extn = expand('%:e')
  if empty(extn)
    let l:fname = a:fname
  else
    let l:fname = a:fname . '.' . extn
  endif
  let [first, last] = [a:firstline, a:lastline]
  let range = first.",".last
  let spaces = matchstr(getline(first),"^ *")
  let buf = @@
  silent exe range."yank"
  let partial = @@
  let @@ = buf
  let old_ai = &ai
  try
    let &ai = 0
    silent exe "norm! :".range."change\<CR>".spaces.a:fname."\<CR>.\<CR>"
  finally
    let &ai = old_ai
  endtry
  execute a:cmd . ' ' . expand('%:p:h') . '/' . l:fname
  let @@ = partial
  silent put
  0delete
  let @@ = buf
  if !empty(spaces)
    silent! exe '%substitute/^' . spaces . '//'
  endif
endfunction

" Scratch {{{1
function! functions#ScratchEdit(cmd, options)
  exe a:cmd tempname()
  setl buftype=nofile bufhidden=wipe nobuflisted
  if !empty(a:options) | exe 'setl' a:options | endif
endfunction

" Cd / Lcd {{{1
function! functions#CdComplete(ArgLead, CmdLine, CursorPos)
  let pattern = empty(a:ArgLead) ? '*/' : '*' . a:ArgLead . '*/'
  return map(globpath(&cdpath, pattern, 1, 1), 'fnamemodify(v:val, ":h:t")')
endfunction

" View {{{1
function! functions#View(cmd)
  redir => output
  silent execute a:cmd
  redir END
  call functions#ScratchEdit('split', '')
  call setline(1, split(output, '\n'))
endfunction
