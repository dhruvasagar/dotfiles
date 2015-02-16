" Fugitive {{{1
command! -bar -nargs=* Ggpull execute 'Gpull' <q-args> 'origin' fugitive#head()
command! -bar -nargs=* Ggpush execute 'Gpush' <q-args> 'origin' fugitive#head()
command! -bar -nargs=* Ggpurr Ggpull --rebase
command! Gpnp silent Ggpull | Ggpush
command! Gprp silent Ggpurr | Ggpush

command! -bar -nargs=+ -complete=customlist,functions#GitFeatureComplete Gfeature Git flow feature <q-args>

" Filter Quickfix / Location list {{{1
command! -bang -nargs=1 -complete=file QFilter call functions#FilterQuickfixList(<bang>0, <q-args>)
command! -bang -nargs=1 -complete=file LFilter call functions#FilterLocationList(<bang>0, <q-args>)

" Extract {{{1
command! -bar -nargs=1 -range SExtract :<line1>,<line2>call functions#Extract(<bang>0,"split",<q-args>)
command! -bar -nargs=1 -range VExtract :<line1>,<line2>call functions#Extract(<bang>0,"vsplit",<q-args>)

" Scratch Buffer {{{1
command! -bar -nargs=* Sedit call functions#ScratchEdit('edit', <q-args>)
command! -bar -nargs=* Ssplit call functions#ScratchEdit('split', <q-args>)
command! -bar -nargs=* Svsplit call functions#ScratchEdit('vsplit', <q-args>)
command! -bar -nargs=* Stabedit call functions#ScratchEdit('tabe', <q-args>)

" Cd / Lcd {{{1
command! -bar -nargs=1 -complete=customlist,functions#CdComplete Cd cd <args>
command! -bar -nargs=1 -complete=customlist,functions#CdComplete Lcd lcd <args>

" View {{{1
command! -bar -nargs=1 -complete=command View call functions#View(<q-args>)

" DiffOff {{{1
command! -bar -nargs=+ -complete=command Bufdo call functions#BufDo(<q-args>)
command! -bar DiffOff silent Bufdo noautocmd diffoff
