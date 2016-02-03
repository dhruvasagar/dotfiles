function! s:SyncFiles()
  :!~/dotfiles/bin/rsync_dotoo_files
endfunction

" augroup Dotoo
"   au!

"   autocmd BufWritePost *.dotoo call s:SyncFiles()
" augroup END

command! SyncDotooFiles call s:SyncFiles()
