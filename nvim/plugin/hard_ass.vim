let s:hard_ass = 1

function! s:ToggleHardAss() abort
  let s:hard_ass = !s:hard_ass

  if s:hard_ass
    map <Up> <Nop>
    map <Down> <Nop>
    map <Left> <Nop>
    map <Right> <Nop>

    imap <Up> <Nop>
    imap <Down> <Nop>
    imap <Left> <Nop>
    imap <Right> <Nop>
  else
    unmap <Up>
    unmap <Down>
    unmap <Left>
    unmap <Right>

    iunmap <Up>
    iunmap <Down>
    iunmap <Left>
    iunmap <Right>
  endif
endfunction

call s:ToggleHardAss()
nnoremap <silent> <Leader>h :call <SID>ToggleHardAss()<CR>
