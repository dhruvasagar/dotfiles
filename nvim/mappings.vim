" List of available normal mode mappings for future use :
"
" * cq, cu, cd, cm, c<CR>, c<, c>, c.
"
" * dq, dr, dy, du, dx, dc, dm, d<, d>, d.
"
" * gy, gz
"
" * yq, yr, yu, yx, yc, ym
"
" * zq, zy, zp

" Use repeat operator with visual selection {{{1
xnoremap . :normal! .<CR>

" To make Y inline with other capitals {{{1
nnoremap Y y$

" Q to :q<CR> a window {{{1
" nmap <expr> <silent> Q empty(maparg('q', 'n')) ? ':q<CR>' : 'q'
nnoremap Q :echo "Use :q instead"<CR>

" Dispatch {{{1
nnoremap d<CR> :Dispatch<CR>

" Gundo {{{1
" nnoremap U :GundoToggle<CR>
nnoremap U :MundoToggle<CR>

" Tabular {{{1
nnoremap <expr> z/ ':Tabular/'.nr2char(getchar()).'<CR>'
xnoremap <expr> z/ ':Tabular/'.nr2char(getchar()).'<CR>'
nnoremap z// :Tabular/
xnoremap z// :Tabular/

" Use ` when ' {{{1
nnoremap ' `

" Test {{{1
nnoremap <Leader>cl :TestLast<CR>
nnoremap <Leader>cf :TestFile<CR>
nnoremap <Leader>cs :TestSuite<CR>
nnoremap <Leader>cc :TestNearest<CR>

" Remap Esc in terminal for NeoVim
if exists(':tnoremap')
  tnoremap <Esc> <C-\><C-n>
endif

nnoremap g< <ESC>vap:Twrite bottom-left<CR>
xnoremap g< :Twrite bottom-left<CR>
nnoremap g> <ESC>vap:Twrite bottom-right<CR>
xnoremap g> :Twrite bottom-right<CR>

nnoremap <, :SidewaysLeft<CR>
nnoremap >, :SidewaysRight<CR>

nnoremap <silent> go :Goyo<CR>

cnoremap <C-P> <UP>

nnoremap <C-P> <cmd>Telescope find_files follow=true<CR>
nnoremap <Leader>sf <cmd>Telescope oldfiles only_cwd=true<CR>
nnoremap <Leader>sb <cmd>Telescope buffers<CR>
nnoremap <Leader>sh <cmd>Telescope help_tags<CR>
nnoremap <Leader>sw <cmd>Telescope grep_string<CR>
nnoremap <Leader>sg <cmd>Telescope live_grep<CR>
nnoremap <Leader>sd <cmd>Telescope diagnostics<CR>
nnoremap <Leader>su <cmd>Telescope undo<CR>

nnoremap c- :.!toilet -w 200 -f small<CR>
nnoremap c+ :.!toilet -w 200 -f standard<CR>
nnoremap cb :.!toilet -w 200 -f term -F border<CR>

nnoremap zS <cmd>TSHighlightCapturesUnderCursor<CR>

nnoremap <Leader>J :TSJJoin<CR>
nnoremap <Leader>S :TSJSplit<CR>
