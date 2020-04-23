setl foldmethod=indent

command! MakeTags !ctags -R --languages=ruby --exclude=.git --exclude=log . $(bundle list --paths)
