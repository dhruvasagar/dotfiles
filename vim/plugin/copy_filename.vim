function! s:CopyFileNameWithLineNumber() range
  if exists('b:git_dir')
    let path = b:git_dir
  else
    let path = fugitive#extract_git_dir(expand('%:p'))
  endif

  if empty(path)
    let path = expand('%:p:h:h')
  else
    let path = fnamemodify(path, ':h:h:t')
  endif
  " Get path relative to path
  let path = fnamemodify(expand('%:p'), ':s?.*' . path . '/??')

  let name_with_lnum = path . ':'
  if a:lastline == a:firstline
    let name_with_lnum .= a:firstline
  else
    let name_with_lnum .= a:firstline . '-' . a:lastline
  endif
  let @+ = name_with_lnum
  echomsg @+ . ' Copied to clipboard'
endfunction

command! -range CopyFileNameWithLineNumber <line1>,<line2>call s:CopyFileNameWithLineNumber()
noremap cy. :CopyFileNameWithLineNumber<CR>
