if exists('g:loaded_scratch')
  finish
endif
let g:loaded_scratch = 1

function! s:ScratchEdit(cmd, options)
  exe a:cmd tempname()
  setl buftype=nofile bufhidden=wipe nobuflisted
  if !empty(a:options) | exe 'setl' a:options | endif
endfunction

let s:filetype_extensions = {
      \ 'ruby': 'rb',
      \ 'python': 'py',
      \ 'javascript': 'js',
      \}

function! s:ScratchFile(cmd, ft)
  let extn = get(s:filetype_extensions, a:ft, a:ft)
  exec a:cmd 'scratch.'.extn
endfunction

command! -bar -nargs=* Sedit call s:ScratchEdit('edit', <q-args>)
command! -bar -nargs=* Ssplit call s:ScratchEdit('split', <q-args>)
command! -bar -nargs=* Svsplit call s:ScratchEdit('vsplit', <q-args>)
command! -bar -nargs=* Stabedit call s:ScratchEdit('tabe', <q-args>)

command! -bar -nargs=1 -complete=filetype Sfedit call s:ScratchFile('edit', <q-args>)
command! -bar -nargs=1 -complete=filetype Sfsplit call s:ScratchFile('split', <q-args>)
command! -bar -nargs=1 -complete=filetype Sfvsplit call s:ScratchFile('vsplit', <q-args>)
command! -bar -nargs=1 -complete=filetype Sftabedit call s:ScratchFile('tabe', <q-args>)
