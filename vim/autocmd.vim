" Auto Command Groups {{{1
augroup vimrcEx "{{{2
  au!

  " Automatically load .vimrc changes
  autocmd BufWritePost $MYVIMRC source $MYVIMRC
augroup END

augroup NeoBundles "{{{2
  au!

  autocmd BufWritePost neo_bundles.vim source %
augroup END

augroup EditJump "{{{2
  au!

  " When editing a file, always jump to the last known cursor position.
  " Don't do it when the position is invalid or when inside an event handler
  " (happens when dropping a file on gvim).
  autocmd BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \   exe "normal g`\"" |
        \ endif
augroup END

augroup SetTitleString "{{{2
  au!

  autocmd BufEnter * call functions#SetTitleString()
augroup END
