if has('nvim')
  let test#strategy = 'neoterm'
else
  let test#strategy = 'vimux'
endif

let test#ruby#rspec#options = {
      \ 'nearest': '--format doc',
      \ 'file':    '--format doc',
      \ 'suite':   '--format progress'
      \}
