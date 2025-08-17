let g:ctrlp_cache_dir = $HOME . '/.cache/ctrlp'
let g:ctrlp_extensions = ['tag', 'buffertag']
let g:ctrlp_show_hidden = 1
let g:ctrlp_custom_ignore = 'node_modules'

if executable('ag')
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
elseif executable('rg')
  let g:ctrlp_user_command = 'rg %s --files --color=never --glob ""'
else
  let g:ctrlp_user_command = ['.git',
        \ 'cd %s && git ls-files . -co --exclude-standard',
        \ 'find %s -type f' ]
endif
