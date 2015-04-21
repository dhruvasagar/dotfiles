if exists('g:loaded_scratch')
  finish
endif
let g:loaded_scratch = 1

function! s:ScratchEdit(cmd, options)
  exe a:cmd tempname()
  setl buftype=nofile bufhidden=wipe nobuflisted
  if !empty(a:options) | exe 'setl' a:options | endif
endfunction

command! -bar -nargs=* Sedit call s:ScratchEdit('edit', <q-args>)
command! -bar -nargs=* Ssplit call s:ScratchEdit('split', <q-args>)
command! -bar -nargs=* Svsplit call s:ScratchEdit('vsplit', <q-args>)
command! -bar -nargs=* Stabedit call s:ScratchEdit('tabe', <q-args>)
