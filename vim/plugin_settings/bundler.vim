augroup Bundler
  au!

  autocmd User Bundler if &makeprg !~# 'bundle' | setl makeprg^=bundle\ exec\ | endif
augroup END
