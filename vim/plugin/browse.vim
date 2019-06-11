function! s:Browse(bang) range
  if exists('b:git_dir')
    let path = b:git_dir
  else
    let path = fugitive#extract_git_dir(expand('%:p'))
  endif

  " Get working directory name
  if empty(path)
    let path = expand('%:p:h')
  else
    let path = fnamemodify(path, ':h:t')
  endif

  " Get path relative to working directory
  let path = fnamemodify(expand('%:p'), ':s?.*' . path . '/??')

  let name_with_lnum = path . ':'
  if a:lastline == a:firstline
    let name_with_lnum .= a:firstline
  else
    let name_with_lnum .= a:firstline . '-' . a:lastline
  endif

  if a:bang
    let @+ = name_with_lnum
    echomsg @+ . ' Copied to clipboard'
  else
    echomsg name_with_lnum
  end
endfunction

command! -bang -range Browse <line1>,<line2>call s:Browse(<bang>0)
