let g:dotoo#home=expand('~/Dropbox/Documents/org-files')
let g:dotoo#agenda#files=['~/Dropbox/Documents/org-files/**/*.org']
let g:dotoo#capture#refile=expand('~/Dropbox/Documents/org-files/refile.org')
let g:dotoo_begin_src_languages=[
      \ 'vim',
      \ 'json',
      \ 'javascript',
      \ 'ruby',
      \ 'typescript',
      \ 'go',
      \ 'rust',
      \ 'java',
      \]

nnoremap <Leader>ww :edit ~/Dropbox/Documents/dotoo-files/notes.dotoo<CR>
nnoremap <Leader>wj :edit ~/Dropbox/Documents/dotoo-files/notes/diary<CR>
