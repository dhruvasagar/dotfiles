augroup VimPipe "{{{2
  au!

  autocmd FileType ruby let b:vimpipe_filetype='ruby' |
        \ if expand('%') =~# '_test\.rb$' |
        \   let b:vimpipe_command='testrb' |
        \ elseif expand('%') =~# '_spec\.rb$' |
        \   let b:vimpipe_command='rspec' |
        \ else |
        \   let b:vimpipe_command='ruby' |
        \ endif
augroup END

let g:vimpipe_invoke_map = '<Leader>pi'
let g:vimpipe_close_map = '<Leader>pc'

let g:vimpipe_javascript_command = 'mocha =(cat)'
let g:vimpipe_javascript_filetype = 'sh'

let g:vimpipe_markdown_command = 'gfm | lynx -dump -stdin'
