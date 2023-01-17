function! Diagnostics() abort
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

function! Winbar() abort
  let winbar = []
  let ds = Diagnostics()
  if !empty(ds)
    call add(winbar, ds)
  endif

  let context = luaeval("require('lspsaga.symbolwinbar'):get_winbar()")
  if !empty(context)
    call add(winbar, "%*Context: " . context . "%*")
  end
  return join(winbar, " | ")
endfunction

set winbar=%{%Winbar()%}
