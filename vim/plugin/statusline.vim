function! s:StatusLineClearVars()
  unlet! b:statusline_git_flag
  unlet! b:statusline_file_name
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

function! StatusLineFileName()
  if !exists('b:statusline_file_name')
    let name = simplify(expand('%:~:.'))
    let ratio = winwidth(0) * 1.0 / len(name)
    if ratio <= 2 && ratio > 1
      let name = pathshorten(name)
    elseif ratio <= 1
      let name = fnamemodify(name, ':t')
    endif
    let b:statusline_file_name = name
  endif
  return b:statusline_file_name
endfunction

augroup StatusLine
  au!

  autocmd WinEnter,CursorHold * call <SID>StatusLineClearVars()
augroup END

set statusline=%(\ \ %{fugitive#head()}\ \|%)
set statusline+=%(\ %{StatusLinePWD()}\ %(%3*%{StatusLinePWDGitFlag()}%*\ %)\|\ %)
set statusline+=%(%r%m\ %)
set statusline+=%3*%(%{StatusLineGitFlag()}\ %)%*
set statusline+=%2*%(%{SyntasticStatuslineFlag()}\ %)%*
set statusline+=%4*%(%{dotoo#clock#summary()}\ %)%*
set statusline+=%1*%{StatusLineFileName()}%*
set statusline+=%<%=
set statusline+=%(%{&filetype}\ \|\ %)
set statusline+=%(%3p%%\ \|\ %)
set statusline+=\ %3l(%L):%-3c
