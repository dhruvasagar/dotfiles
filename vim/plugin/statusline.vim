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
    if !empty(b:statusline_pwd_git_flag)
      let b:statusline_pwd_git_flag .= ' '
    endif
  endif
  return b:statusline_pwd_git_flag
endfunction

function! StatusLineFileName()
  let pre = ''
  let pat = '://'
  let name = expand('%:~:.')
  if name =~# pat
    let pre = name[:stridx(name, pat) + len(pat)-1] . '...'
    let name = name[stridx(name, pat) + len(pat):]
  elseif empty(name) && &filetype ==# 'netrw'
    let name = fnamemodify(b:netrw_curdir, ':~:.')
  endif
  let name = simplify(name)
  let ratio = winwidth(0) / len(name)
  if ratio <= 2 && ratio > 1
    let name = pathshorten(name)
  elseif ratio <= 1
    let name = fnamemodify(name, ':t')
  endif
  return pre . name
endfunction

augroup StatusLine
  au!

  autocmd WinEnter,CursorHold * call <SID>StatusLineClearVars()
augroup END

function! StatusLineALE() abort
  if !exists(':ALE*')
    return ''
  endif
  let l:s = []
  let ale = ale#statusline#Count(bufnr('%'))
  if ale['error'] > 0
    call add(l:s, 'E: ' . ale['error'])
  endif
  if ale['warning'] > 0
    call add(l:s, 'W: ' . ale['warning'])
  endif
  if ale['total'] > 0
    call add(l:s, 'T: ' . ale['total'])
  endif
  if !empty(l:s)
    return '[ALE '.join(l:s, ',').']'
  endif
  return ''
endfunction

set statusline=%(\ %5*%{zoom#statusline()}%*\ \|%)
set statusline+=%(\ %{fugitive#head()}\ \|%)
set statusline+=%(\ %{StatusLinePWD()}\ %(%3*%{StatusLinePWDGitFlag()}%*%)\|\ %)
set statusline+=%(%r%m\ %)

set statusline+=%3*%(%{StatusLineGitFlag()}\ %)%*
set statusline+=%2*%(%{StatusLineALE()}\ %)%*
set statusline+=%1*%{StatusLineFileName()}\ %*

if exists(':Tagbar')
  set statusline+=%1*ϝ:%{tagbar#currenttag('%s','')}\ %*
endif

set statusline+=%4*%(%{dotoo#clock#summary()}\ %)%*
set statusline+=%<%=
set statusline+=%(%{&filetype}\ \|\ %)
set statusline+=%(%3p%%\ \|\ %)
set statusline+=%3l(%L):%-3c
