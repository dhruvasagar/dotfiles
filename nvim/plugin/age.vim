function! s:age_encrypt() abort
  silent exec '!cat' expand('%') '| age -e -i ~/.ssh/age/key.txt -o' expand('%')
endfunction

function! s:age_decrypt() abort
  silent exec '!cat' expand('%') '| age -d -i ~/.ssh/age/key.txt -o' expand('%')
endfunction

command! AgeFileEncrypt call s:age_encrypt()
command! AgeFileDecrypt call s:age_decrypt()
