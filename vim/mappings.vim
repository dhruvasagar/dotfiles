" Use repeat operator with visual selection {{{1
xnoremap . :normal! .<CR>

" To make Y inline with other capitals {{{1
nnoremap Y y$
nnoremap <silent> Q :if !empty(maparg('q', 'n')) <Bar> exe 'normal q' <Bar> else <Bar> exec 'normal! ZQ' <Bar> endif<CR>

" Focus on current buffer {{{1
nnoremap <C-W>O :tab split<CR>

" Dispatch {{{1
nnoremap <Leader>D :Dispatch<CR>

" Fugitive {{{1
nnoremap gsl :Glog<CR>
nnoremap gsd :Gdiff<CR>
nnoremap gsb :Gblame<CR>
nnoremap gsw :Gwrite<CR>
nnoremap gsC :Gcommit<CR>
nnoremap gst :Gstatus<CR>
nnoremap gscd :Gcd<Bar>pwd<CR>
nnoremap gsld :Glcd<Bar>pwd<CR>

" CtrlP {{{1
nnoremap <C-p><C-b> :CtrlPBuffer<CR>
nnoremap <C-p><C-m> :CtrlPMRUFiles<CR>
nnoremap <C-p><C-l> :CtrlPLastMode<CR>

" Gundo {{{1
nnoremap U :GundoToggle<CR>

" Tabular mapping {{{1
xnoremap z// :Tabular/
xnoremap z/= :Tabular/=<CR>
xnoremap z/: :Tabular/:<CR>

" Search {{{1
function! s:Vword()
  return getline('.')[col("'<")-1:col("'>")-1]
endfunction

xnoremap <silent> * <Esc>/\v<<C-R>=<SID>Vword()<CR>><CR>
xnoremap <silent> g* <Esc>/\v<C-R>=<SID>Vword()<CR><CR>

xnoremap <silent> # o<Esc>?\v<<C-R>=<SID>Vword()<CR>><CR>
xnoremap <silent> g# o<Esc>?\v<C-R>=<SID>Vword()<CR><CR>

nnoremap <silent> g// :grep -w <cword> <C-R>=getcwd()<CR><CR>
nnoremap <silent> g/* :grep <cword> <C-R>=getcwd()<CR><CR>

xnoremap <silent> g// :<C-U>grep -w <C-R>=<SID>Vword()<CR> <C-R>=getcwd()<CR><CR>
xnoremap <silent> g/* :<C-U>grep <C-R>=<SID>Vword()<CR> <C-R>=getcwd()<CR><CR>

" Execute {{{1
nmap <silent> gx <Plug>(quickrun)
xmap <silent> gx <Plug>(quickrun)

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
nnoremap gB :OpenURL <cfile><CR>
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

" Hard Mode {{{1
map <Up> <Nop>
map <Down> <Nop>
map <Left> <Nop>
map <Right> <Nop>

imap <Up> <Nop>
imap <Down> <Nop>
imap <Left> <Nop>
imap <Right> <Nop>

" Find it! {{{1
nnoremap <Leader>f :find 

" VimShell {{{1
xnoremap <silent> <C-C><C-C> :<C-U>call vimshell#interactive#send(substitute(join(getline("'<","'>"), ' '), '\t', ' ', 'g'))<CR>
nnoremap <silent> <C-C><C-C> :<C-U>call vimshell#interactive#send(substitute(join(getline("'{","'}"), ' '), '\t', ' ', 'g'))<CR>

" Use ` when '
nnoremap ' `
