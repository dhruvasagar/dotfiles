stty -ixon

export LC_ALL="en_US.UTF-8"
export LANG="en_US.UTF-8"

export EDITOR='vim'
export VISUAL='vim'

# For directory completion
export CDPATH=$HOME:$HOME/code:$HOME/code/vim_plugins:$HOME/code/oss
export CDPATH=$CDPATH:$HOME/code/moove

# Source Prezto.
[[ -f ~/.zprezto/init.zsh ]] && source ~/.zprezto/init.zsh

# Customize to your needs...
export PATH=$HOME/bin:$HOME/dotfiles/bin:/usr/local/bin:/usr/local/sbin:/usr/local/opt/coreutils/libexec/gnubin:$PATH

# rbenv
[[ -n "$(rbenv 2>/dev/null)" ]] && eval "$(rbenv init -)"
export PATH=$HOME/.rbenv/bin:$HOME/.rbenv/shims:$PATH

# TERM with at least 256 colors (24 bit on supported terminals)
export TERM=xterm-256color

# cassandra
export CASSANDRA_HOME=~/cassandra
export PATH=$CASSANDRA_HOME/bin:$PATH

# always prefer tmux
tssh() {
  ssh $1 -t 'tmux has-session && tmux attach -t dhruva || tmux new -s dhruva'
}
compdef _ssh tssh=ssh

rmd() {
  pandoc $1 | lynx -stdin
}

# nvm
# export NVM_DIR=~/.nvm
# NVM_SH=$(brew --prefix nvm 2>/dev/null || echo $NVM_DIR)/nvm.sh
# [[ -f $NVM_SH ]] && source $NVM_SH
# [[ -n "$(npm bin 2>/dev/null)" ]] && export PATH=$(npm bin):$PATH # Add local node_modules binaries to path

# go lang
export GOPATH=$HOME/.go
export PATH=$GOPATH/bin:/usr/local/go/bin:$PATH
[[ -f ~/.gvm/scripts/gvm ]] && source ~/.gvm/scripts/gvm

platform=$(uname | awk '{print tolower($0)}')
if [[ "$platform" == "darwin" ]]; then
  docker-init() {
    docker_vm=$(docker-machine ls | awk '{print $1}' | sed '1d' | head -1)
    if [[ -z "docker_vm" ]]; then
      docker-machine create --driver virtualbox docker
    else
      docker-machine start $docker_vm
    fi
    eval $(docker-machine env $docker_vm)
  }

  export NDK_HOME=/usr/local/Cellar/android-ndk/r10e
  export ANDROID_HOME=~/Library/Android/sdk
  export STUDIO_JDK=/Library/Java/JavaVirtualMachines/jdk1.8.0_66.jdk

  launchctl setenv NDK_HOME "$NDK_HOME"
  launchctl setenv ANDROID_HOME "$ANDROID_HOME"
fi

PATH="/Users/dhruvasagar/perl5/bin${PATH:+:${PATH}}"; export PATH;
PERL5LIB="/Users/dhruvasagar/perl5/lib/perl5${PERL5LIB:+:${PERL5LIB}}"; export PERL5LIB;
PERL_LOCAL_LIB_ROOT="/Users/dhruvasagar/perl5${PERL_LOCAL_LIB_ROOT:+:${PERL_LOCAL_LIB_ROOT}}"; export PERL_LOCAL_LIB_ROOT;
PERL_MB_OPT="--install_base \"/Users/dhruvasagar/perl5\""; export PERL_MB_OPT;
PERL_MM_OPT="INSTALL_BASE=/Users/dhruvasagar/perl5"; export PERL_MM_OPT;

export WORKON_HOME=$HOME/.virtualenvs
[[ -f /usr/local/bin/virtualenvwrapper_lazy ]] && source /usr/local/bin/virtualenvwrapper_lazy.sh

# export BROWSER=w3m

if [ $commands[kubectl] ]; then
  source <(kubectl completion zsh)
fi
