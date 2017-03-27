augroup vimrcEx "{{{1
  au!

  " Automatically load .vimrc changes
  autocmd BufWritePost $MYVIMRC nested source $MYVIMRC
augroup END

augroup Plugins "{{{1
  au!

  autocmd BufWritePost plugs.vim source % | filetype plugin indent on
  " autocmd BufWritePost neo_bundles.vim source % | filetype plugin indent on
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

augroup CursorLine "{{{1
  au!

  autocmd BufEnter,WinEnter * setl cursorline
  autocmd BufLeave,WinLeave * setl nocursorline
augroup END

augroup Gemfile "{{{1
  au!

  autocmd BufWritePost Gemfile Dispatch bundle install
augroup END

augroup LargeFiles "{{{1
  au!

  autocmd BufWinEnter * if line2byte(line("$") + 1) > 500000 | syntax clear | endif
augroup END

if has('nvim')
  augroup Terminal "{{{1
    au!

    autocmd TermOpen * setl nolist
  augroup END
end
