function! s:FilterQuickfixList(bang, pattern)
  let [cmp, and_or] = a:bang ? ['!~#', '&&'] : ['=~#', '||']
  call setqflist(filter(getqflist(), "bufname(v:val.bufnr) " . cmp . " a:pattern " . and_or . " v:val.text " . cmp . " a:pattern"))
endfunction

function! s:FilterLocationList(bang, pattern)
  let [cmp, and_or] = a:bang ? ['!~#', '&&'] : ['=~#', '||']
  call setloclist('%', filter(getloclist('%'), "bufname(v:val.bufnr) " . cmp . " a:pattern " . and_or . " v:val.text " . cmp . " a:pattern"))
endfunction

command! -bang -nargs=1 -complete=file QFilter call s:FilterQuickfixList(<bang>0, <q-args>)
command! -bang -nargs=1 -complete=file LFilter call s:FilterLocationList(<bang>0, <q-args>)
