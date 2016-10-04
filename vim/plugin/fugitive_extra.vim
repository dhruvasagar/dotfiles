function! s:GitShortRefNames(names, full_name) "{{{1
  if a:full_name
    let expr = 'v:val[11:]'
  else
    let expr = 'v:val[strridx(v:val, "/")+1:]'
  endif
  return map(a:names, expr)
endfunction

function! s:GitComplete(ArgLead, Cmdline, Cursor, ...) "{{{1
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

function! s:GitExtraComplete(ArgLead, CmdLine, Cursor, type) "{{{1
  " if (empty(a:ArgLead) || a:ArgLead =~? '^f\%[inish]$') && a:CmdLine !~? 'finish\s*$'
  "   return ['finish']
  " else
    return s:GitComplete(a:ArgLead, a:CmdLine, a:Cursor, 'branch', a:type)
  " endif
endfunction

function! s:GitBugComplete(ArgLead, CmdLine, Cursor) "{{{1
  return s:GitExtraComplete(a:ArgLead, a:CmdLine, a:Cursor, 'bug')
endfunction

function! s:GitFeatureComplete(ArgLead, CmdLine, Cursor) "{{{1
  return s:GitExtraComplete(a:ArgLead, a:CmdLine, a:Cursor, 'feature')
endfunction

function! s:GitRefactorComplete(ArgLead, CmdLine, Cursor) "{{{1
  return s:GitExtraComplete(a:ArgLead, a:CmdLine, a:Cursor, 'refactor')
endfunction

" Commands & Mappings {{{1
command! -bar -nargs=* Gpurr Gpull --rebase
command! Gpnp silent Gpull | Gpush
command! Gprp silent Gpurr | Gpush

command! -bar -nargs=+ -complete=customlist,s:GitFeatureComplete Gfeature Git flow feature <q-args>

nnoremap gsl :Glog<CR>
nnoremap gsd :Gdiff<CR>
nnoremap gse :Gedit<CR>
nnoremap gsb :Gblame<CR>
nnoremap gsw :Gwrite<CR>
nnoremap gsC :Gcommit<CR>
nnoremap gst :Gstatus<CR>
nnoremap gscd :Gcd<Bar>pwd<CR>
nnoremap gsld :Glcd<Bar>pwd<CR>
