function! s:SyncFiles()
  :!~/dotfiles/bin/rsync_dotoo_files
endfunction

command! SyncDotooFiles call s:SyncFiles()
