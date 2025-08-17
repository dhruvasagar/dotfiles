set termguicolors

let g:vim_home = expand('~/.config/nvim/')

let config_list = [
      \ 'config.vim',
      \ 'autocmd.vim',
      \ 'commands.vim',
      \ 'mappings.vim',
      \ 'plugin_settings/*.vim'
      \]
for files in config_list
  for f in glob(g:vim_home.files, 1, 1)
    exec 'source' f
  endfor
endfor

packadd cfilter
packadd termdebug

lua require 'init'

" Set at the end to work around 'exrc'
set secure
