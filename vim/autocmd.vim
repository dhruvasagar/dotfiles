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

let g:LargeFile = 70000
function! LargeFile()
  set ft=
endfunction
augroup LargeFiles "{{{1
  au!

  autocmd BufRead * let f = getfsize(expand('<afile>')) | if f > g:LargeFile || f == -2 | call LargeFile() | endif
augroup END

augroup Terminal "{{{1
  au!

  if has('nvim')
    autocmd TermOpen * setl nolist
  else
    autocmd TerminalOpen * setl nolist
  end
augroup END
