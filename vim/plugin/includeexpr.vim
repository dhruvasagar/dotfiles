function! IncludeExpr()
  if v:fname =~? '^this\.'
    return substitute(v:fname, '^this\.', '', 'g')
  endif
  if !exists('g:Abolish') || empty(g:Abolish) | return | endif
  let l:fname = findfile(g:Abolish.camelcase(v:fname))
  if empty(l:fname)
    let l:fname = findfile(g:Abolish.mixedcase(v:fname))
  endif
  if empty(l:fname)
    let l:fname = findfile(g:Abolish.snakecase(v:fname))
  endif
  if empty(l:fname)
    let l:fname = findfile(g:Abolish.uppercase(v:fname))
  endif
  if empty(l:fname)
    let l:fname = findfile(g:Abolish.dashcase(v:fname))
  endif
  if empty(l:fname)
    let l:fname = findfile(g:Abolish.dotcase(v:fname))
  endif
  return l:fname
endfunction
set includeexpr=IncludeExpr()
