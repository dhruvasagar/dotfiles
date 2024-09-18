function! s:lookup() abort
  let word = expand('<cword>')
  let buf = nvim_create_buf(v:false, v:true)
  let lines = systemlist('def '. word)
  call nvim_buf_set_lines(buf, 0, -1, v:true, lines)
  let win = nvim_open_win(buf, v:true, {
        \ 'relative': 'cursor',
        \ 'col': 0,
        \ 'row': 1,
        \ 'width': 100,
        \ 'height': 20,
        \ 'anchor': 'NW',
        \ 'style': 'minimal',
        \ 'border': 'rounded',
        \ 'title': 'Definition of word: "'. word . '"',
        \ 'title_pos': 'center',
        \ 'fixed': v:true,
        \})
  call nvim_set_option_value('wrap', v:false, {'win': win})
    call nvim_set_option_value('winhl', 'Normal:MyHighlight', {'win': win})
endfunction

nnoremap <silent> <Leader>def :call <SID>lookup()<CR>
