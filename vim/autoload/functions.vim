" Title String for tabs {{{1
function! functions#SetTitleString()
  set titlestring=%f\ %m
  set titlestring+=\ -\ [%{split(substitute(getcwd(),$HOME,'~',''),'/')[-1]}]
endfunction


" Fugitive {{{1
function! functions#GitExecInPath(cmd, ...) "{{{2
  if a:0
    let path = a:1
  elseif exists('b:git_dir')
    let path = b:git_dir
    let path = fnamemodify(path, ':h')
  else
    let path = FugitiveExtractGitDir('.')
    let path = fnamemodify(path, ':h')
  endif

  return system('cd ' . fnameescape(path) . '; ' . a:cmd)
endfunction

function! functions#TableizeMarkdown() range
  let line = a:firstline
  while line < a:lastline
    " First subheading
    if getline(line) =~# '^##'
      break
    endif
    let line += 1
  endwhile

  let lines = getline(line, a:lastline)
  let columns = []
  let column = []
  for line in lines
    if empty(line) | continue | endif

    if !empty(column) && line =~# '^##' " Column Header
      call add(columns, column)
      let column = []
    endif

    call add(column, line)
  endfor
  call add(columns, column) " last column

  let rows = []
  for icol in range(len(columns))
    let row = []
    for irow in range(len(columns))
      if irow < len(columns) && icol < len(columns[irow])
        let val = substitute(columns[irow][icol], '^##\|^- ', '', '')
        call add(row, val)
      else
        call add(row, '')
      endif
    endfor
    call add(rows, row)
  endfor

  exec ':' . a:firstline . ',' . a:lastline 'delete'

  let lines = map(rows, {_, row -> '|' . join(row, '|') . '|'})
  call insert(lines, '', 1)
  call append(a:firstline - 1, lines)
  call tablemode#table#AddBorder(a:firstline + 1)
  call tablemode#table#Realign(a:firstline)
endfunction
