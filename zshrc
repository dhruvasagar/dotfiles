stty -ixon

export EDITOR='vim'
export VISUAL='vim'

# For directory completion
export CDPATH=$HOME:$HOME/code:$HOME/code/vim_plugins:$HOME/code/modus

# Source Prezto.
[[ -f ~/.zprezto/init.zsh ]] && source ~/.zprezto/init.zsh

# Customize to your needs...
export PATH=$HOME/bin:$HOME/dotfiles/bin:/usr/local/bin:/usr/local/sbin:/usr/local/opt/coreutils/libexec/gnubin:$PATH

# rbenv
[[ -n "$(rbenv 2>/dev/null)" ]] && eval "$(rbenv init -)"
export PATH=$HOME/.rbenv/bin:$HOME/.rbenv/shims:$PATH

# tmux & TERM issue fix
export TERM=xterm-256color
[[ -n $TMUX ]] && export TERM=screen-256color

# cassandra
export CASSANDRA_HOME=~/cassandra
export PATH=$CASSANDRA_HOME/bin:$PATH

# always prefer tmux
tssh() {
  ssh $1 -t 'tmux has-session && tmux attach -t dhruva || tmux new -s dhruva'
}
compdef _ssh tssh=ssh

# nvm
export NVM_DIR=~/.nvm
NVM_SH=$(brew --prefix nvm 2>/dev/null || echo $NVM_DIR)/nvm.sh
[[ -f $NVM_SH ]] && source $NVM_SH
[[ -n "$(npm bin 2>/dev/null)" ]] && export PATH=$(npm bin):$PATH # Add local node_modules binaries to path

# go lang
export GOPATH=$HOME/.go
export PATH=$GOPATH/bin:/usr/local/go/bin:$PATH
[[ -f ~/.gvm/scripts/gvm ]] && source ~/.gvm/scripts/gvm
