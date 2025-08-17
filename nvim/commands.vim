function! s:makeTags() abort
  if executable('ctags')
    !ctags -R --execlude=.git
  elseif executable('ptags')
    !ptags
  endif
endfunction

command! MakeTags call s:makeTags()
