ZSH=$HOME/.oh-my-zsh
ZSH_THEME="amuse"
EDITOR='vim'

plugins=(git git-extras rbenv rails rake gem bundler node npm vagrant tmux)

source $ZSH/oh-my-zsh.sh

PATH=$HOME/bin:$HOME/dotfiles/bin:$PATH
PATH=$HOME/.rbenv/bin:$PATH # Add rbenv
eval "$(rbenv init - --no-rehash)"

export CDPATH=$CDPATH:$HOME/code:$HOME/code/pulse

# tmux & TERM issue fix
export TERM=xterm-256color
[[ -n $TMUX ]] && export TERM=screen-256color

# Java environment
JAVA_HOME=/usr/local/java/jdk1.7.0_21/
PATH=$PATH:$JAVA_HOME/bin

# tmux
PS1="$PS1"'$([ -n "$TMUX" ] && tmux setenv TMUXPWD_$(tmux display -p "#D" | tr -d %) "$PWD")'

PAGER=~/code/vimpager/vimpager
alias less=$PAGER
alias zless=$PAGER

# git aliases
alias glgg1="git log --graph --all --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset) %C(white)%s%C(reset) %C(bold white)— %an%C(reset)%C(bold yellow)%d%C(reset)' --abbrev-commit --date=relative"
compdef _git glgg1=git-log
alias glgg2="git log --graph --all --format=format:'%C(bold blue)%h%C(reset) - %C(bold cyan)%aD%C(reset) %C(bold green)(%ar)%C(reset)%C(bold yellow)%d%C(reset)%n''          %C(white)%s%C(reset) %C(bold white)— %an%C(reset)' --abbrev-commit"
compdef _git glgg2=git-log

# Added by the Heroku Toolbelt
export PATH=/usr/local/heroku/bin:$PATH

alias act_mail='VMAIL_HOME=~/.vmail/activesphere vmail'
alias pulse_mail='VMAIL_HOME=~/.vmail/pulseenergy vmail'

stty -ixon
