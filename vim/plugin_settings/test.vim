if has('nvim')
  let test#strategy = 'neoterm'

  let g:neoterm_test_status = {
        \ 'running': '♪',
        \ 'success': '✓',
        \ 'failed': '✗'
        \}
  let g:neoterm_run_tests_bg = 1
  let g:neoterm_raise_when_tests_fail = 1
  let g:neoterm_close_when_tests_succeed = 1
else
  let test#strategy = 'vimux'
  let g:VimuxOrientation = 'h'
endif

let test#ruby#rspec#options = {
      \ 'nearest': '--format doc',
      \ 'file':    '--format doc',
      \ 'suite':   '--format progress'
      \}
