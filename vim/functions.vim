function! s:SetTitleString() "{{{1
  set titlestring=%f\ %m
  set titlestring+=\ -\ [%{split(substitute(getcwd(),$HOME,'~',''),'/')[-1]}]
endfunction

augroup SetTitleString "{{{2
  au!

  autocmd BufEnter * call s:SetTitleString()
augroup END

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

" Fugitive gems {{{1
function! s:GitShortRefNames(names, full_name) "{{{2
  if a:full_name
    let expr = 'v:val[11:]'
  else
    let expr = 'v:val[strridx(v:val, "/")+1:]'
  endif
  return map(a:names, expr)
endfunction

function! s:GitExecInPath(cmd) "{{{2
  if exists('b:git_dir')
    let path = b:git_dir
  else
    let path = fugitive#extract_git_dir('.')
  endif
  let path = fnamemodify(path, ':h')

	return system('cd ' . path . '; ' . a:cmd)
endfunction

function! s:GitComplete(ArgLead, Cmdline, Cursor, ...) "{{{2
  let refs = 'refs/heads/'
  if a:0 == 1 && a:1 !=? 'branch'
    let refs = 'refs/' . a:1
    let full_name = 1
  elseif a:0 == 2 && a:1 ==? 'branch'
    let refs = refs . a:2
    let full_name = 0
  endif

  let cmd = 'git for-each-ref --format="%(refname)" ' . refs
  if !empty(a:ArgLead)
    let cmd = cmd . ' | sed "s/.*\/\(.*\)/\1/" | grep ^' . a:ArgLead . ' 2>/dev/null'
  endif
  return s:GitShortRefNames(split(s:GitExecInPath(cmd)), full_name)
endfunction

function! s:GitExtraComplete(ArgLead, CmdLine, Cursor, type) "{{{2
  if (empty(a:ArgLead) || a:ArgLead =~? '^f\%[inish]$') && a:CmdLine !~? 'finish\s*$'
    return ['finish']
  else
    return s:GitComplete(a:ArgLead, a:CmdLine, a:Cursor, 'branch', a:type)
  endif
endfunction

function! s:GitBugComplete(ArgLead, CmdLine, Cursor) "{{{2
  return s:GitExtraComplete(a:ArgLead, a:CmdLine, a:Cursor, 'bug')
endfunction

function! s:GitFeatureComplete(ArgLead, CmdLine, Cursor) "{{{2
  return s:GitExtraComplete(a:ArgLead, a:CmdLine, a:Cursor, 'feature')
endfunction

function! s:GitRefactorComplete(ArgLead, CmdLine, Cursor) "{{{2
  return s:GitExtraComplete(a:ArgLead, a:CmdLine, a:Cursor, 'refactor')
endfunction

" Commands {{{2
command! -bar -nargs=* Gpull execute 'Git pull' <q-args> 'origin' fugitive#head()
command! -bar -nargs=* Gpush execute 'Git push' <q-args> 'origin' fugitive#head()
command! -bar -nargs=* Gpurr execute 'Git pull --rebase' <q-args> 'origin' fugitive#head()
command! Gpnp silent Gpull | Gpush
command! Gprp silent Gpurr | Gpush

command! -bar -nargs=+ -complete=customlist,s:GitBugComplete Gbug Git bug <q-args>
command! -bar -nargs=+ -complete=customlist,s:GitFeatureComplete Gfeature Git feature <q-args>
command! -bar -nargs=+ -complete=customlist,s:GitRefactorComplete Grefactor Git refactor <q-args>

" Random Number {{{1
function! Rand()
  return str2nr(matchstr(reltimestr(reltime()), '\v\.@<=\d+')[1:])
endfunction

" Filter quickfix list {{{1
function! s:FilterQuickfixList(bang, pattern)
	let [cmp, and_or] = a:bang ? ['!~#', '&&'] : ['=~#', '||']
  call setqflist(filter(getqflist(), "bufname(v:val.bufnr) " . cmp . " a:pattern " . and_or . " v:val.text " . cmp . " a:pattern"))
endfunction
command! -bang -nargs=1 -complete=file QFilter call s:FilterQuickfixList(<bang>0, <q-args>)

" Filter location list {{{1
function! s:FilterLocationList(bang, pattern)
	let [cmp, and_or] = a:bang ? ['!~#', '&&'] : ['=~#', '||']
  call setloclist('%', filter(getloclist('%'), "bufname(v:val.bufnr) " . cmp . " a:pattern " . and_or . " v:val.text " . cmp . " a:pattern"))
endfunction
command! -bang -nargs=1 -complete=file LFilter call s:FilterLocationList(<bang>0, <q-args>)

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

" Extract {{{1
function! s:Extract(bang,cmd,fname) range abort
  let extn = expand('%:e')
  let l:fname = a:fname . '.' . extn
  let [first, last] = [a:firstline, a:lastline]
  let range = first.",".last
  let spaces = matchstr(getline(first),"^ *")
  let buf = @@
  silent exe range."yank"
  let partial = @@
  let @@ = buf
  let old_ai = &ai
  try
    let &ai = 0
    silent exe "norm! :".range."change\<CR>".spaces.a:fname."\<CR>.\<CR>"
  finally
    let &ai = old_ai
  endtry
  execute a:cmd . ' ' . expand('%:p:h') . '/' . l:fname
  let @@ = partial
  silent put
  0delete
  let @@ = buf
  if !empty(spaces)
    silent! exe '%substitute/^' . spaces . '//'
  endif
endfunction
command! -bar -nargs=1 -range SExtract :<line1>,<line2>call s:Extract(<bang>0,"split",<q-args>)
command! -bar -nargs=1 -range VExtract :<line1>,<line2>call s:Extract(<bang>0,"vsplit",<q-args>)
