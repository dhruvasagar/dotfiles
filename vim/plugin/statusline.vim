function! s:StatusLineClearVars()
  unlet! b:statusline_git_flag
  unlet! b:statusline_pwd_git_flag
  if exists('b:statusline_pwd') && fnamemodify(getcwd(), ':t') !=# b:statusline_pwd
    unlet b:statusline_pwd
  endif
endfunction

function! StatusLinePWD()
  if !exists('b:statusline_pwd')
    let b:statusline_pwd = fnamemodify(getcwd(), ':t')
  endif
  return b:statusline_pwd
endfunction

function! StatusLineGitFlag()
  if !exists('b:statusline_git_flag')
    if !file_readable(expand('%'))
      let b:statusline_git_flag = ''
    else
      let b:statusline_git_flag = functions#GitExecInPath('git status --porcelain ' . expand('%') . " 2>/dev/null | awk '{print $1}'")[:-2]
    endif
  endif
  return b:statusline_git_flag
endfunction

function! StatusLinePWDGitFlag()
  if !exists('b:statusline_pwd_git_flag')
    let b:statusline_pwd_git_flag = functions#GitExecInPath("git status --porcelain 2>/dev/null | head -1 | awk '{print $1}'", getcwd())[:-2]
  endif
  return b:statusline_pwd_git_flag
endfunction

augroup StatusLine
  au!

  autocmd WinEnter,CursorHold * call <SID>StatusLineClearVars()
augroup END

set statusline=%(\ \ %{fugitive#head()}\ \|%)
set statusline+=%(\ %{StatusLinePWD()}\ %(%3*%{StatusLinePWDGitFlag()}%*\ %)\|\ %)
set statusline+=%1*%{simplify(expand('%:~:.'))}%*\ 
set statusline+=%(%r%m\ %)
set statusline+=%3*%{StatusLineGitFlag()}%*
set statusline+=\ %2*%{SyntasticStatuslineFlag()}%*
set statusline+=%<%=
set statusline+=%(%{&filetype}\ \|\ %)
set statusline+=%(%3p%%\ \|\ %)
set statusline+=\ %3l(%L):%-3c
