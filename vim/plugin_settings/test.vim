if has('nvim')
  let test#strategy = 'neoterm'
else
  let test#strategy = 'vimux'
  let g:VimuxOrientation = 'h'
endif

let test#ruby#rspec#options = {
      \ 'nearest': '--format doc',
      \ 'file':    '--format doc',
      \ 'suite':   '--format progress'
      \}
