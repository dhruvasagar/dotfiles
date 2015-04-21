if has('nvim')
  let test#strategy = 'neovim'
else
  let test#strategy = 'vimux'
endif
