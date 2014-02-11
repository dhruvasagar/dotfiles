" Use repeat operator with visual selection {{{1
vnoremap . :normal! .<CR>

" To make Y inline with other capitals {{{1
nnoremap Y y$
nnoremap <silent> Q :if maparg('q', 'n') <Bar> exe 'normal q' <Bar> else <Bar> exec 'normal! ZQ' <Bar> endif<CR>

" text-object for complete file {{{1
onoremap <silent> af :<C-U>normal! ggVG<CR>

" Clear search highlights {{{1
nnoremap <Leader>/ :nohl<CR>

" Focus on current buffer {{{1
nnoremap <C-W>O :tab split<CR>

" Dispatch {{{1
nnoremap di :Dispatch<CR>

" Fugitive {{{1
nnoremap <Leader>gl :Glog<CR>
nnoremap <Leader>gd :Gdiff<CR>
nnoremap <Leader>gb :Gblame<CR>
nnoremap <Leader>gw :Gwrite<CR>
nnoremap <Leader>gc :Gcommit<CR>
nnoremap <Leader>gs :Gstatus<CR>
nnoremap <Leader>gcd :Gcd<Bar>pwd<CR>
nnoremap <Leader>gld :Glcd<Bar>pwd<CR>

" Gundo {{{1
nnoremap U :GundoToggle<CR>

" Tabular mapping {{{1
xnoremap z// :Tabularize/
xnoremap z/= :Tabularize/=<CR>
xnoremap z/: :Tabularize/:<CR>

" Search {{{1
function! s:Vcword()
  return getline('.')[col("'<")-1:col("'>")-1]
endfunction

xnoremap <silent> * <Esc>/\<<C-R>=<SID>Vcword()<CR>\><CR>
xnoremap <silent> g* <Esc>/<C-R>=<SID>Vcword()<CR><CR>

xnoremap <silent> # o<Esc>?\<<C-R>=<SID>Vcword()<CR>\><CR>
xnoremap <silent> g# o<Esc>?<C-R>=<SID>Vcword()<CR><CR>

nnoremap <silent> g// :grep -w <C-R>=expand('<cword>')<CR> <C-R>=getcwd()<CR><CR>
nnoremap <silent> g/* :grep <C-R>=expand('<cword>')<CR> <C-R>=getcwd()<CR><CR>

xnoremap <silent> g// :<C-U>grep -w <C-R>=<SID>Vcword()<CR> <C-R>=getcwd()<CR><CR>
xnoremap <silent> g/* :<C-U>grep <C-R>=<SID>Vcword()<CR> <C-R>=getcwd()<CR><CR>

" Execute {{{1
nmap <silent> gX <Plug>(quickrun)
xmap <silent> gX <Plug>(quickrun)

" OpenUrl {{{1
function! s:OpenURL(url)
  if has("win32")
    exe "!start cmd /cstart /b ".a:url.""
  elseif $DISPLAY !~ '^\w'
    exe "silent !sensible-browser \"".a:url."\""
  else
    exe "silent !sensible-browser -T \"".a:url."\""
  endif
  redraw!
endfunction
command! -nargs=1 OpenURL :call s:OpenURL(<q-args>)
" open URL under cursor in browser
nnoremap gb :OpenURL <cfile><CR>
nnoremap gA :OpenURL http://www.answers.com/<cword><CR>
nnoremap gG :OpenURL http://www.google.com/search?q=<cword><CR>
nnoremap gW :OpenURL http://en.wikipedia.org/wiki/Special:Search?search=<cword><CR>

" Copy current file with line number {{{1
function! s:CopyFileNameWithLineNumber() range
  if exists('b:git_dir')
    let path = b:git_dir
  else
    let path = fugitive#extract_git_dir(expand('%:p'))
  endif

  if empty(path)
    let path = expand('%:p:h:h')
  else
    let path = fnamemodify(path, ':h:h:t')
  endif
	" Get path relative to path
	let path = fnamemodify(expand('%:p'), ':s?.*' . path . '/??')

  let name_with_lnum = path . ':'
  if a:lastline == a:firstline
    let name_with_lnum .= a:firstline
  else
    let name_with_lnum .= a:firstline . '-' . a:lastline
  endif
  let @+ = name_with_lnum
  echomsg @+ . ' Copied to clipboard'
endfunction
command! -range CopyFileNameWithLineNumber <line1>,<line2>call s:CopyFileNameWithLineNumber()
noremap y. :CopyFileNameWithLineNumber<CR>
