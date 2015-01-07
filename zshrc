# Source Prezto.
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

export EDITOR='vim'
export VISUAL='vim'

# Customize to your needs...
PATH=$HOME/bin:$HOME/dotfiles/bin:/usr/local/bin:/usr/local/sbin:$PATH
PATH=$HOME/.rbenv/bin:$HOME/.rbenv/shims:$PATH # Add rbenv
eval "$(rbenv init -)"

export CDPATH=$CDPATH:$HOME:$HOME/code:$HOME/code/vim_plugins

# tmux & TERM issue fix
export TERM=xterm-256color
[[ -n $TMUX ]] && export TERM=screen-256color

# tmux
PS1="$PS1"'$([ -n "$TMUX" ] && tmux setenv TMUXPWD_$(tmux display -p "#D" | tr -d %) "$PWD")'

# git aliases
alias glgg1="git log --graph --all --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(bold white)— %an%C(reset)%C(bold yellow)%d%C(reset)' --abbrev-commit --date=relative"
compdef _git glgg1=git-log
alias glgg2="git log --graph --all --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d%C(reset)%n''          %C(white)%s%C(reset) %C(bold white)— %an%C(reset)' --abbrev-commit"
compdef _git glgg2=git-log

alias npm-exec='PATH=$(npm bin):$PATH'

# Added by the Heroku Toolbelt
# export PATH=/usr/local/heroku/bin:$PATH

stty -ixon

export CASSANDRA_HOME=~/cassandra
export PATH=$CASSANDRA_HOME/bin:$PATH
# export JAVA_HOME=`/usr/libexec/java_home`
