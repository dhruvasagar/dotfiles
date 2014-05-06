function! NeatFoldText() "{{{1
  let line = ' ' . substitute(getline(v:foldstart), '^\s*"\?\s*\|\s*"\?\s*{{' . '{\d*\s*', '', 'g') . ' '
  let lines_count = v:foldend - v:foldstart + 1
  let lines_count_text = '| ' . printf("%10s", lines_count . ' lines') . ' |'
  let foldchar = matchstr(&fillchars, 'fold:\zs.')
  let foldtextstart = strpart('+' . repeat(foldchar, v:foldlevel*2) . line, 0, (winwidth(0)*2)/3)
  let foldtextend = lines_count_text . repeat(foldchar, 8)
  let foldtextlength = strlen(substitute(foldtextstart . foldtextend, '.', 'x', 'g')) + &foldcolumn
  return foldtextstart . repeat(foldchar, winwidth(0)-foldtextlength) . foldtextend
endfunction
set foldtext=NeatFoldText()

" MyTabLine {{{1
" Based on : http://www.offensivethinking.org/data/dotfiles/vimrc
function! MyTabLine()
  let s = ''
  for i in range(tabpagenr('$'))
    let tabnr = i + 1 " range() starts at 0
    let winnr = tabpagewinnr(tabnr)
    let buflist = tabpagebuflist(tabnr)
    let bufnr = buflist[winnr - 1]
    let bufname = fnamemodify(bufname(bufnr), ':t')

    let s .= '%' . tabnr . 'T'
    let s .= (tabnr == tabpagenr() ? '%#TabLineSel#' : '%#TabLine#')
    let s .= ' ' . tabnr

    let n = tabpagewinnr(tabnr,'$')
    if n > 1 | let s .= ':' . n | endif

    let s .= empty(bufname) ? ' [No Name] ' : ' ' . bufname . ' '

    let bufmodified = getbufvar(bufnr, "&mod")
    if bufmodified | let s .= '+ ' | endif
  endfor
  let s .= '%#TabLineFill#'
  return s
endfunction
set tabline=%!MyTabLine()

" IncludeExpr {{{1
function! IncludeExpr()
	if v:fname =~? '^this\.'
		return substitute(v:fname, '^this\.', '', 'g')
	endif
  if !exists('g:Abolish') || empty(g:Abolish) | return | endif
  let l:fname = findfile(g:Abolish.camelcase(v:fname))
  if empty(l:fname)
    let l:fname = findfile(g:Abolish.mixedcase(v:fname))
  endif
  if empty(l:fname)
    let l:fname = findfile(g:Abolish.snakecase(v:fname))
  endif
  if empty(l:fname)
    let l:fname = findfile(g:Abolish.uppercase(v:fname))
  endif
  if empty(l:fname)
    let l:fname = findfile(g:Abolish.dashcase(v:fname))
  endif
  if empty(l:fname)
    let l:fname = findfile(g:Abolish.dotcase(v:fname))
  endif
  return l:fname
endfunction
set includeexpr=IncludeExpr()

" Statusline {{{1
set statusline=%(\ \ %{fugitive#head()}\ \|%)
set statusline+=%(\ %{StatusLinePWD()}\ %(%3*%{StatusLinePWDGitFlag()}%*\ %)\|\ %)
set statusline+=%1*%f%*\ 
set statusline+=%(%r%m\ %)
set statusline+=%3*%{StatusLineGitFlag()}%*
set statusline+=\ %2*%{SyntasticStatuslineFlag()}%*
set statusline+=%<%=
set statusline+=%(%{&filetype}\ \|\ %)
set statusline+=%(%3p%%\ \|\ %)
set statusline+=\ %3l(%L):%-3c

augroup StatusLine
  au!

  autocmd WinEnter,CursorHold * call <SID>StatusLineClearVars()
augroup END

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
