" Use `[g` and `]g` to navigate diagnostics
nmap <silent> [g <Plug>(coc-diagnostic-prev)
nmap <silent> ]g <Plug>(coc-diagnostic-next)

" GoTo code navigation.
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

function! s:show_documentation() abort
  if !CocAction('doHover')
    normal! K
  endif
endfunction

" Use K to show documentation in preview window.
nnoremap <silent> K :call <SID>show_documentation()<CR>

" Introduce function text object
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
xmap if <Plug>(coc-funcobj-i)
xmap af <Plug>(coc-funcobj-a)
omap if <Plug>(coc-funcobj-i)
omap af <Plug>(coc-funcobj-a)

let g:coc_global_extensions = [
      \ 'coc-sh',
      \ 'coc-css',
      \ 'coc-sql',
      \ 'coc-json',
      \ 'coc-yaml',
      \ 'coc-reason',
      \ 'coc-vimlsp',
      \ 'coc-graphql',
      \ 'coc-swagger',
      \ 'coc-tsserver',
      \ 'coc-solargraph',
      \ 'coc-rust-analyzer',
      \ ]
