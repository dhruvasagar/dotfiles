let g:vimshell_prompt = '$ '
let g:vimshell_user_prompt = 'fnamemodify(getcwd(), ":~")'

xnoremap <silent> <C-C><C-C> :VimShellSendString<CR>
nnoremap <silent> <C-C><C-C> :'{,'}VimShellSendString<CR>
