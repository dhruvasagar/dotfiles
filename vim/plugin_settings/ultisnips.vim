let g:UltiSnips = {}
let g:UltiSnips.ExpandTrigger = '<Tab>'
let g:UltiSnips.JumpForwardTrigger = '<Tab>'
let g:UltiSnips.JumpBackwardTrigger = '<S-Tab>'
let g:UltiSnips.always_use_first_snippet = 1
let g:UltiSnips.Ultisnips_ft_filter = {
      \ 'default' : {'filetypes': ['all', 'FILETYPE'] },
			\ 'jade': {'filetypes': ['all', 'css', 'javascript'] },
			\ 'haml': {'filetypes': ['all', 'haml', 'css', 'javascript'] },
      \ 'html': {'filetypes': ['all', 'html', 'css', 'javascript'] },
      \ 'eruby': {'filetypes': ['all', 'eruby', 'css', 'html', 'javascript'] }
      \ }
let g:UltiSnips.snipmate_ft_filter = {
      \ 'default': {'filetypes': ['_', 'FILETYPE'] },
			\ 'jade': {'filetypes': ['all', 'css', 'javascript'] },
      \ 'haml': {'filetypes': ['_', 'haml', 'css', 'javascript'] },
      \ 'html': {'filetypes': ['_', 'html', 'css', 'javascript'] },
      \ 'eruby': {'filetypes': ['_', 'eruby', 'css', 'html', 'javascript'] }
      \ }
