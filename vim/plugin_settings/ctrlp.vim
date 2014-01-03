let g:ctrlp_map = '<C-p><C-p>'
let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'
let g:ctrlp_extensions = ['tag', 'buffertag']
let g:ctrlp_show_hidden = 1
let g:ctrlp_custom_ignore = 'node_modules'

if executable('ag')
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif

nnoremap <C-p><C-b> :CtrlPBuffer<CR>
nnoremap <C-p><C-m> :CtrlPMRUFiles<CR>
nnoremap <C-p><C-l> :CtrlPLastMode<CR>
