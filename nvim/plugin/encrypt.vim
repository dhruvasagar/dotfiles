if exists('g:loaded_encrypt')
  finish
endif
let g:loaded_encrypt = 1

if !exists('g:encrypt_credentials_file')
  let g:encrypt_credentials_file = expand('~/Dropbox/Documents/encryption-keys.txt')
endif

function! s:BackupCreds(pass) abort
  let fname = expand('%:p')
  let credline = printf("%s : %s", fname, a:pass)
  exec 'split' g:encrypt_credentials_file
  " Delete the line if it already exists
  exec 'global#^'.fnameescape(fname).'#d'
  call append(line('$'), credline)
  write
  quit
endfunction

function s:Encrypt() abort
  set cryptmethod=blowfish2
  let pass = inputsecret("Enter password: ")
  exec 'set key='.pass
  write
  call s:BackupCreds(pass)
endfunction

command! Encrypt call s:Encrypt()
