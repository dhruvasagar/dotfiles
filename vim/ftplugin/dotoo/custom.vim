augroup Dotoo
  au!

  autocmd BufWritePost *.dotoo !~/dotfiles/bin/rsync_dotoo_files
augroup END
