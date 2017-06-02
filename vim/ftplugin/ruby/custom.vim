setl foldmethod=indent

command! ECtags :!ctags -R --languages=ruby --exclude=.git --exclude=log . $(bundle list --paths)
