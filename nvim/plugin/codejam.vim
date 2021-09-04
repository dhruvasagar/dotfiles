if exists('g:loaded_codejam')
  finish
endif
let g:loaded_codejam = 1

let g:codejam_template = {
      \ 'ruby': [
      \   '#!/usr/bin/env ruby',
      \   '',
      \   'def log(msg, i)',
      \   '  puts "Case ##{i+1}: #{msg}"',
      \   'end',
      \   ''
      \ ],
      \ 'go': [
      \   'package main',
      \   '',
      \   'import "fmt"',
      \   '',
      \   'func main() {',
      \      '',
      \   '}'
      \ ]
      \}

function! s:CodejamTemplate(...)
  let b:codejam_ft = a:0 && !empty(a:1) ? a:1 : 'ruby'
  exec 'setf' b:codejam_ft
  new
  call setline(1, get(g:codejam_template, b:codejam_ft, ''))
  normal! G
endfunction

command! -nargs=? CJNew call s:CodejamTemplate(<q-args>)
