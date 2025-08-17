function! s:puml_preview() abort
  let buf = nvim_create_buf(v:false, v:true)
  let file = expand('%')
  let fname = expand('%:r')
  call system('plantuml -ttxt ' . file)
  let lines = readfile(fname . '.atxt')
  call nvim_buf_set_lines(buf, 0, -1, v:true, lines)

  let gheight = nvim_list_uis()[0].height
  let gwidth = nvim_list_uis()[0].width
  let width = 100
  let height = 30
  let win = nvim_open_win(buf, v:true, {
        \ 'relative': 'editor',
        \ 'col': (gwidth - width) * 0.5,
        \ 'row': (gheight - height) * 0.5,
        \ 'width': width,
        \ 'height': height,
        \ 'anchor': 'NW',
        \ 'style': 'minimal',
        \ 'border': 'rounded',
        \ 'title': 'PlantUML Preview: "'. file . '"',
        \ 'title_pos': 'center',
        \ 'fixed': v:true,
        \})
  call nvim_set_option_value('wrap', v:false, {'win': win})
    call nvim_set_option_value('winhl', 'Normal:MyHighlight', {'win': win})
endfunction

nnoremap <silent> <buffer> <Leader>p :call <SID>puml_preview()<CR>
