let g:vimwiki_global_ext = 0
let g:vimwiki_table_mappings = 0
let g:vimwiki_table_auto_fmt = 0

let wiki = {
      \ 'path': '~/Dropbox/Documents/vimwiki',
      \ 'nested_syntaxes': {'ledger': 'ledger', 'sh': 'sh', 'bash': 'bash', 'ruby': 'ruby', 'golang': 'go'},
      \ 'ext': '.wiki',
      \}
let g:vimwiki_list = [wiki]
