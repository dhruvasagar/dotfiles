" Fugitive
command! -bar -nargs=* Gpull execute 'Git pull' <q-args> 'origin' fugitive#head()
command! -bar -nargs=* Gpush execute 'Git push' <q-args> 'origin' fugitive#head()
command! -bar -nargs=* Gpurr execute 'Git pull --rebase' <q-args> 'origin' fugitive#head()
command! Gpnp silent Gpull | Gpush
command! Gprp silent Gpurr | Gpush

command! -bar -nargs=+ -complete=customlist,functions#GitBugComplete Gbug Git bug <q-args>
command! -bar -nargs=+ -complete=customlist,functions#GitFeatureComplete Gfeature Git feature <q-args>
command! -bar -nargs=+ -complete=customlist,functions#GitRefactorComplete Grefactor Git refactor <q-args>

" Filter Quickfix / Location list
command! -bang -nargs=1 -complete=file QFilter call functions#FilterQuickfixList(<bang>0, <q-args>)
command! -bang -nargs=1 -complete=file LFilter call functions#FilterLocationList(<bang>0, <q-args>)

" Extract
command! -bar -nargs=1 -range SExtract :<line1>,<line2>call functions#Extract(<bang>0,"split",<q-args>)
command! -bar -nargs=1 -range VExtract :<line1>,<line2>call functions#Extract(<bang>0,"vsplit",<q-args>)

" Scratch Buffer
command! -bar -nargs=* Sedit call functions#ScratchEdit('edit', <q-args>)
command! -bar -nargs=* Ssplit call functions#ScratchEdit('split', <q-args>)
command! -bar -nargs=* Svsplit call functions#ScratchEdit('vsplit', <q-args>)
command! -bar -nargs=* Stabedit call functions#ScratchEdit('tabe', <q-args>)

" Cd / Lcd
command! -bar -nargs=1 -complete=customlist,functions#CdComplete Cd cd <args>
command! -bar -nargs=1 -complete=customlist,functions#CdComplete Lcd lcd <args>

" View
command! -bar -nargs=1 -complete=command Redir call functions#View(<q-args>)
