" Vim syntax file
" Language: Guitar Tabs
" Maintainer: Josh Hartigan
" Latest Revision: 21 September 2014

if exists("b:current_syntax")
  finish
endif

" Regex matches
syn match SectionHeader      /\v.+\:$/

syn match StringLetter       /\v^[A-Za-z]\|/

syn match Empty              /\v-/

syn match Number             /x|\d\d?/

syn match SlideUp            /\/\//
syn match SlideDown          /\\\\/
syn match HammerOn           /h[^a-zA-Z]/
syn match PullOff            /p\d[^a-zA-Z]/
syn match Bend               /b\d[^a-zA-Z]/
syn match Vibrato            /\dv\d\?-/

syn match TabComment         /(.*)/

" Highlight types
highlight link SlideUp       Function
highlight link SlideDown     Function
highlight link HammerOn      Function
highlight link PullOff       Function
highlight link Bend          Function
highlight link Vibrato       Function

highlight link Empty         Comment

highlight link Number        Type

highlight link SectionHeader PreProc

highlight link StringLetter  String

highlight link TabComment    Comment
