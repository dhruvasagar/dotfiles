let g:vimshell_prompt =  '$ '
let g:vimshell_user_prompt = 'fnamemodify(getcwd(), ":~")'

function! s:FocusVimShellExecute(cmd)
  if !empty(a:cmd) | let b:vimshell_execute_cmd = a:cmd | endif
  if exists('b:vimshell_execute_cmd')
    execute 'VimShellExecute ' . b:vimshell_execute_cmd
  end
endfunction
command! -bar -nargs=* FocusVimShellExecute call s:FocusVimShellExecute(<q-args>)
nnoremap <Leader>F :FocusVimShellExecute<CR>
