function! s:diagnostics() abort
  let ds = []
  let eCount = luaeval("#vim.diagnostic.get(0, { severity = vim.diagnostic.severity.ERROR })")
  let wCount = luaeval("#vim.diagnostic.get(0, { severity = vim.diagnostic.severity.WARN })")
  if eCount > 0
    call add(ds, "%#DiagnosticSignError#E: " . eCount . "%*")
  endif
  if wCount > 0
    call add(ds, "%#DiagnosticSignWarn#W: " . wCount . "%*")
  endif
  if !empty(ds)
    return "Diagnostics: " . join(ds, ", ")
  endif
  return ""
endfunction

function! s:gitStatus() abort
  let gstatus = StatusLineGitFlag()
  if !empty(gstatus)
    return "Git: " . gstatus . "%*"
  endif
  return ""
endfunction

function! s:context() abort
  let ctx = luaeval("require('lspsaga.symbolwinbar'):get_winbar()")
  if !empty(ctx)
    return "Context: " . ctx . "%*"
  endif
  return ""
endfunction

function! s:addNotEmpty(list, item) abort
  if !empty(a:item)
    call add(a:list, a:item)
  endif
endfunction

function! Winbar() abort
  let winbar = []
  call s:addNotEmpty(winbar, s:gitStatus())
  call s:addNotEmpty(winbar, s:diagnostics())
  call s:addNotEmpty(winbar, s:context())
  return join(winbar, " | ")
endfunction

set winbar=%{%Winbar()%}
