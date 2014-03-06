let g:vimshell_prompt = '$ '
let g:vimshell_user_prompt = 'fnamemodify(getcwd(), ":~")'

augroup VimShell
  au!

  autocmd FileType int-node setl ft=int-node.javascript nonu
augroup END
