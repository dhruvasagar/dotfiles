if has('nvim')
  let test#strategy = 'neoterm'
else
  let test#strategy = 'vimux'
endif

let g:VimuxOrientation = "h"
