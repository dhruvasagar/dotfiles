function! s:FilterList(list, bang, pattern)
  let list = deepcopy(a:list)
  let [cmp, and_or] = a:bang ? ['!~#', '&&'] : ['=~#', '||']
  return filter(a:list, "bufname(v:val.bufnr) " . cmp . " a:pattern " . and_or . " v:val.text " . cmp . " a:pattern")
endfunction

function! s:FilterQuickfixList(bang, pattern)
  call setqflist(s:FilterList(getqflist(), a:bang, a:pattern))
endfunction

function! s:FilterLocationList(bang, pattern)
  call setloclist(0, s:FilterList(getloclist(0), a:bang, a:pattern))
endfunction

command! -bang -nargs=1 -complete=file Qfilter call s:FilterQuickfixList(<bang>0, <q-args>)
command! -bang -nargs=1 -complete=file Lfilter call s:FilterLocationList(<bang>0, <q-args>)
