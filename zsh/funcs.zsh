tssh () {
  ssh $1 -t 'tmux has-session && tmux attach -t dhruva || tmux new -s dhruva'
}
autoload -Uz compinit
compinit
compdef _ssh tssh=ssh

go-cover () {
  t="/tmp/go-cover.$$.tmp"
  go test -coverprofile=$t $@ && go tool cover -html=$t && unlink $t
}

npm-do() {
  (PATH=$(npm bin):$PATH; eval $@;)
}
