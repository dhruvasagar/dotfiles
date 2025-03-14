augroup vimrcEx "{{{1
  au!

  " Automatically load .vimrc changes
  autocmd BufWritePost $MYVIMRC nested source $MYVIMRC
augroup END

augroup Packer "{{{1
  au!

  autocmd BufWritePost packer.lua source % | LspRestart | PackerCompile
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

let g:LargeFile = 200000
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

augroup CronTab "{{{1
  au!

  autocmd BufWritePost ~/dotfiles/crontab !updatecron
augroup END

augroup Kitty
  au!

  autocmd BufWritePost ~/.config/kitty/kitty.conf :silent !kill -SIGUSR1 $(pgrep kitty)
augroup END
