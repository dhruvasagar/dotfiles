function! IncludeExpr()
  if v:fname =~? '^this\.'
    return substitute(v:fname, '^this\.', '', 'g')
  endif
  if !exists('g:Abolish') || empty(g:Abolish) | return | endif
  return findfile(g:Abolish.snakecase(v:fname))
endfunction
set includeexpr=IncludeExpr()
