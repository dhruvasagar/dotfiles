augroup vimrcEx "{{{1
  au!

  " Automatically load .vimrc changes
  autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END

augroup NeoBundles "{{{1
  au!

  autocmd BufWritePost neo_bundles.vim source % | filetype plugin indent on
augroup END

augroup EditJump "{{{1
  au!

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \   exe "normal g`\"" |
        \ endif
augroup END

augroup SetTitleString "{{{1
  au!

  autocmd BufEnter * call functions#SetTitleString()
augroup END
