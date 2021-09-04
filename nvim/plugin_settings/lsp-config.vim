" LSP config (the mappings used in the default file don't quite work right)
nnoremap <silent> gd <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <silent> gD <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <silent> gr <cmd>lua vim.lsp.buf.references()<CR>
nnoremap <silent> gi <cmd>lua vim.lsp.buf.implementation()<CR>
nnoremap <silent> K <cmd>lua vim.lsp.buf.hover()<CR>
nnoremap <silent> <C-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
nnoremap <silent> [d <cmd>lua vim.lsp.diagnostic.goto_prev()<CR>
nnoremap <silent> ]d <cmd>lua vim.lsp.diagnostic.goto_next()<CR>

nnoremap <silent> <Leader>lf <cmd>lua vim.lsp.buf.formatting_sync(nil, 100)<CR>
nnoremap <silent> <Leader>ltd <cmd>lua vim.lsp.buf.type_definition()<CR>
nnoremap <silent> <Leader>lrn <cmd>lua vim.lsp.buf.rename()<CR>
nnoremap <silent> <Leader>lca <cmd>lua vim.lsp.buf.code_action()<CR>

" auto-format
" autocmd BufWritePre * silent! lua vim.lsp.buf.formatting_sync(nil, 100)
