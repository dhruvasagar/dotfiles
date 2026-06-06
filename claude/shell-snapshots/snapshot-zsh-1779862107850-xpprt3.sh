# Snapshot file
# Unset all aliases to avoid conflicts with functions
unalias -a 2>/dev/null || true
# Functions
__bun_dynamic_comp () {
	local comp="" 
	for arg in scripts
	do
		local line
		while read -r line
		do
			local name="$line" 
			local desc="$line" 
			name="${name%$'\t'*}" 
			desc="${desc/*$'\t'/}" 
			echo
		done <<< "$arg"
	done
	return $comp
}
add-zsh-hook () {
	emulate -L zsh
	local -a hooktypes
	hooktypes=(chpwd precmd preexec periodic zshaddhistory zshexit zsh_directory_name) 
	local usage="Usage: add-zsh-hook hook function\nValid hooks are:\n  $hooktypes" 
	local opt
	local -a autoopts
	integer del list help
	while getopts "dDhLUzk" opt
	do
		case $opt in
			(d) del=1  ;;
			(D) del=2  ;;
			(h) help=1  ;;
			(L) list=1  ;;
			([Uzk]) autoopts+=(-$opt)  ;;
			(*) return 1 ;;
		esac
	done
	shift $(( OPTIND - 1 ))
	if (( list ))
	then
		typeset -mp "(${1:-${(@j:|:)hooktypes}})_functions"
		return $?
	elif (( help || $# != 2 || ${hooktypes[(I)$1]} == 0 ))
	then
		print -u$(( 2 - help )) $usage
		return $(( 1 - help ))
	fi
	local hook="${1}_functions" 
	local fn="$2" 
	if (( del ))
	then
		if (( ${(P)+hook} ))
		then
			if (( del == 2 ))
			then
				set -A $hook ${(P)hook:#${~fn}}
			else
				set -A $hook ${(P)hook:#$fn}
			fi
			if (( ! ${(P)#hook} ))
			then
				unset $hook
			fi
		fi
	else
		if (( ${(P)+hook} ))
		then
			if (( ${${(P)hook}[(I)$fn]} == 0 ))
			then
				typeset -ga $hook
				set -A $hook ${(P)hook} $fn
			fi
		else
			typeset -ga $hook
			set -A $hook $fn
		fi
		autoload $autoopts -- $fn
	fi
}
aerospace-cleanup () {
	aerospace list-windows --all | grep -e '.*|.*| $' | awk '{print $1}' | while read -r id
	do
		echo "Closing ghost window ID: $id"
		aerospace close --window-id "$id"
	done
}
asdf () {
	case $1 in
		("shell") if ! shift
			then
				printf '%s\n' 'asdf: Error: Failed to shift' >&2
				return 1
			fi
			eval "$(asdf export-shell-version sh "$@")" ;;
		(*) command asdf "$@" ;;
	esac
}
asdf-current-info () {
	if [ ! $+commands[asdf] ]
	then
		return
	fi
	asdf current 2>&1 | awk '{print substr($1,1,2)":"$2","}' | tr '\n' ' ' | sed 's/,\s$/\n/'
}
bashcompinit () {
	# undefined
	builtin autoload -XUz
}
calc () {
	echo "$@" | bc -l
}
compaudit () {
	# undefined
	builtin autoload -XUz /usr/share/zsh/5.9/functions
}
compdef () {
	local opt autol type func delete eval new i ret=0 cmd svc 
	local -a match mbegin mend
	emulate -L zsh
	setopt extendedglob
	if (( ! $# ))
	then
		print -u2 "$0: I need arguments"
		return 1
	fi
	while getopts "anpPkKde" opt
	do
		case "$opt" in
			(a) autol=yes  ;;
			(n) new=yes  ;;
			([pPkK]) if [[ -n "$type" ]]
				then
					print -u2 "$0: type already set to $type"
					return 1
				fi
				if [[ "$opt" = p ]]
				then
					type=pattern 
				elif [[ "$opt" = P ]]
				then
					type=postpattern 
				elif [[ "$opt" = K ]]
				then
					type=widgetkey 
				else
					type=key 
				fi ;;
			(d) delete=yes  ;;
			(e) eval=yes  ;;
		esac
	done
	shift OPTIND-1
	if (( ! $# ))
	then
		print -u2 "$0: I need arguments"
		return 1
	fi
	if [[ -z "$delete" ]]
	then
		if [[ -z "$eval" ]] && [[ "$1" = *\=* ]]
		then
			while (( $# ))
			do
				if [[ "$1" = *\=* ]]
				then
					cmd="${1%%\=*}" 
					svc="${1#*\=}" 
					func="$_comps[${_services[(r)$svc]:-$svc}]" 
					[[ -n ${_services[$svc]} ]] && svc=${_services[$svc]} 
					[[ -z "$func" ]] && func="${${_patcomps[(K)$svc][1]}:-${_postpatcomps[(K)$svc][1]}}" 
					if [[ -n "$func" ]]
					then
						_comps[$cmd]="$func" 
						_services[$cmd]="$svc" 
					else
						print -u2 "$0: unknown command or service: $svc"
						ret=1 
					fi
				else
					print -u2 "$0: invalid argument: $1"
					ret=1 
				fi
				shift
			done
			return ret
		fi
		func="$1" 
		[[ -n "$autol" ]] && autoload -rUz "$func"
		shift
		case "$type" in
			(widgetkey) while [[ -n $1 ]]
				do
					if [[ $# -lt 3 ]]
					then
						print -u2 "$0: compdef -K requires <widget> <comp-widget> <key>"
						return 1
					fi
					[[ $1 = _* ]] || 1="_$1" 
					[[ $2 = .* ]] || 2=".$2" 
					[[ $2 = .menu-select ]] && zmodload -i zsh/complist
					zle -C "$1" "$2" "$func"
					if [[ -n $new ]]
					then
						bindkey "$3" | IFS=$' \t' read -A opt
						[[ $opt[-1] = undefined-key ]] && bindkey "$3" "$1"
					else
						bindkey "$3" "$1"
					fi
					shift 3
				done ;;
			(key) if [[ $# -lt 2 ]]
				then
					print -u2 "$0: missing keys"
					return 1
				fi
				if [[ $1 = .* ]]
				then
					[[ $1 = .menu-select ]] && zmodload -i zsh/complist
					zle -C "$func" "$1" "$func"
				else
					[[ $1 = menu-select ]] && zmodload -i zsh/complist
					zle -C "$func" ".$1" "$func"
				fi
				shift
				for i
				do
					if [[ -n $new ]]
					then
						bindkey "$i" | IFS=$' \t' read -A opt
						[[ $opt[-1] = undefined-key ]] || continue
					fi
					bindkey "$i" "$func"
				done ;;
			(*) while (( $# ))
				do
					if [[ "$1" = -N ]]
					then
						type=normal 
					elif [[ "$1" = -p ]]
					then
						type=pattern 
					elif [[ "$1" = -P ]]
					then
						type=postpattern 
					else
						case "$type" in
							(pattern) if [[ $1 = (#b)(*)=(*) ]]
								then
									_patcomps[$match[1]]="=$match[2]=$func" 
								else
									_patcomps[$1]="$func" 
								fi ;;
							(postpattern) if [[ $1 = (#b)(*)=(*) ]]
								then
									_postpatcomps[$match[1]]="=$match[2]=$func" 
								else
									_postpatcomps[$1]="$func" 
								fi ;;
							(*) if [[ "$1" = *\=* ]]
								then
									cmd="${1%%\=*}" 
									svc=yes 
								else
									cmd="$1" 
									svc= 
								fi
								if [[ -z "$new" || -z "${_comps[$1]}" ]]
								then
									_comps[$cmd]="$func" 
									[[ -n "$svc" ]] && _services[$cmd]="${1#*\=}" 
								fi ;;
						esac
					fi
					shift
				done ;;
		esac
	else
		case "$type" in
			(pattern) unset "_patcomps[$^@]" ;;
			(postpattern) unset "_postpatcomps[$^@]" ;;
			(key) print -u2 "$0: cannot restore key bindings"
				return 1 ;;
			(*) unset "_comps[$^@]" ;;
		esac
	fi
}
compdump () {
	# undefined
	builtin autoload -XUz /usr/share/zsh/5.9/functions
}
compgen () {
	local opts prefix suffix job OPTARG OPTIND ret=1 
	local -a name res results jids
	local -A shortopts
	emulate -L sh
	setopt kshglob noshglob braceexpand nokshautoload
	shortopts=(a alias b builtin c command d directory e export f file g group j job k keyword u user v variable) 
	while getopts "o:A:G:C:F:P:S:W:X:abcdefgjkuv" name
	do
		case $name in
			([abcdefgjkuv]) OPTARG="${shortopts[$name]}"  ;&
			(A) case $OPTARG in
					(alias) results+=("${(k)aliases[@]}")  ;;
					(arrayvar) results+=("${(k@)parameters[(R)array*]}")  ;;
					(binding) results+=("${(k)widgets[@]}")  ;;
					(builtin) results+=("${(k)builtins[@]}" "${(k)dis_builtins[@]}")  ;;
					(command) results+=("${(k)commands[@]}" "${(k)aliases[@]}" "${(k)builtins[@]}" "${(k)functions[@]}" "${(k)reswords[@]}")  ;;
					(directory) setopt bareglobqual
						results+=(${IPREFIX}${PREFIX}*${SUFFIX}${ISUFFIX}(N-/)) 
						setopt nobareglobqual ;;
					(disabled) results+=("${(k)dis_builtins[@]}")  ;;
					(enabled) results+=("${(k)builtins[@]}")  ;;
					(export) results+=("${(k)parameters[(R)*export*]}")  ;;
					(file) setopt bareglobqual
						results+=(${IPREFIX}${PREFIX}*${SUFFIX}${ISUFFIX}(N)) 
						setopt nobareglobqual ;;
					(function) results+=("${(k)functions[@]}")  ;;
					(group) emulate zsh
						_groups -U -O res
						emulate sh
						setopt kshglob noshglob braceexpand
						results+=("${res[@]}")  ;;
					(hostname) emulate zsh
						_hosts -U -O res
						emulate sh
						setopt kshglob noshglob braceexpand
						results+=("${res[@]}")  ;;
					(job) results+=("${savejobtexts[@]%% *}")  ;;
					(keyword) results+=("${(k)reswords[@]}")  ;;
					(running) jids=("${(@k)savejobstates[(R)running*]}") 
						for job in "${jids[@]}"
						do
							results+=(${savejobtexts[$job]%% *}) 
						done ;;
					(stopped) jids=("${(@k)savejobstates[(R)suspended*]}") 
						for job in "${jids[@]}"
						do
							results+=(${savejobtexts[$job]%% *}) 
						done ;;
					(setopt | shopt) results+=("${(k)options[@]}")  ;;
					(signal) results+=("SIG${^signals[@]}")  ;;
					(user) results+=("${(k)userdirs[@]}")  ;;
					(variable) results+=("${(k)parameters[@]}")  ;;
					(helptopic)  ;;
				esac ;;
			(F) COMPREPLY=() 
				local -a args
				args=("${words[0]}" "${@[-1]}" "${words[CURRENT-2]}") 
				() {
					typeset -h words
					$OPTARG "${args[@]}"
				}
				results+=("${COMPREPLY[@]}")  ;;
			(G) setopt nullglob
				results+=(${~OPTARG}) 
				unsetopt nullglob ;;
			(W) results+=(${(Q)~=OPTARG})  ;;
			(C) results+=($(eval $OPTARG))  ;;
			(P) prefix="$OPTARG"  ;;
			(S) suffix="$OPTARG"  ;;
			(X) if [[ ${OPTARG[0]} = '!' ]]
				then
					results=("${(M)results[@]:#${OPTARG#?}}") 
				else
					results=("${results[@]:#$OPTARG}") 
				fi ;;
		esac
	done
	print -l -r -- "$prefix${^results[@]}$suffix"
}
compinit () {
	# undefined
	builtin autoload -XUz /usr/share/zsh/5.9/functions
}
compinstall () {
	# undefined
	builtin autoload -XUz /usr/share/zsh/5.9/functions
}
complete () {
	emulate -L zsh
	local args void cmd print remove
	args=("$@") 
	zparseopts -D -a void o: A: G: W: C: F: P: S: X: a b c d e f g j k u v p=print r=remove
	if [[ -n $print ]]
	then
		printf 'complete %2$s %1$s\n' "${(@kv)_comps[(R)_bash*]#* }"
	elif [[ -n $remove ]]
	then
		for cmd
		do
			unset "_comps[$cmd]"
		done
	else
		compdef _bash_complete\ ${(j. .)${(q)args[1,-1-$#]}} "$@"
	fi
}
docker-dev () {
	port="$1" 
	project=${PWD##*/} 
	if [ -z "$port" ]
	then
		docker run -dt -v $PWD:/opt/projects --name $project debian:10 bash
	else
		docker run -dt -v $PWD:/opt/projects -p $port:$port --name $project debian:10 bash
	fi
}
edit-command-line () {
	# undefined
	builtin autoload -XUz
}
git-change-summary () {
	if ! command git rev-parse 2> /dev/null
	then
		return
	fi
	changes=$(git status --porcelain | head -1 2>/dev/null) 
	if [ -n "$changes" ]
	then
		if [ "${changes:0:1}" = "?" ]
		then
			echo " %F{cyan}?%f"
		else
			echo " %F{red}✗%f"
		fi
	else
		echo " %F{green}✔%f"
	fi
}
git-grouped-log () {
	while read -r -u 9 since name
	do
		until=$(date -j -v+1d -f '%Y-%m-%d' $since +%Y-%m-%d) 
		echo "$since $name"
		echo
		GIT_PAGER=cat git log --no-merges --committer="$name" --since="$since 00:00:00 +0000" --until="$until 00:00:00 +0000" --format='  * [%h] %s'
		echo
	done 9< <(git log --no-merges --format=$'%cd %cn' --date=short | sort --unique --reverse)
}
git-info () {
	if ! command git rev-parse 2> /dev/null
	then
		return
	fi
	ref=$(command git symbolic-ref HEAD 2>/dev/null) 
	if [ -n "$ref" ]
	then
		echo " on %F{magenta}${ref#refs/heads/}%f$(git-change-summary)"
	fi
}
go-cover () {
	t="/tmp/go-cover.$$.tmp" 
	go test -coverprofile=$t $@ && go tool cover -html=$t && unlink $t
}
kubectl () {
	if ! type __start_kubectl > /dev/null 2>&1
	then
		source <(command kubectl completion zsh)
	fi
	command kubectl "$@"
}
lcflow () {
	period=${1-"this month"} 
	ledger --current --invert --real --wide -s -X $ -S "amount" -p "$period" reg income expenses
}
mac-cam () {
	ffplay -f avfoundation -framerate 30 -i "0" -fflags nobuffer -noborder -left 1120 -top 660
}
mkdcd () {
	[[ -n "$1" ]] && mkdir -p "$1" && builtin cd "$1"
}
play_faah () {
	local exit_code=$? 
	if [ $exit_code -ne 0 ]
	then
		afplay ~/Music/faaa.mp3 > /dev/null 2>&1 &|
	fi
}
profzsh () {
	shell=${1-$SHELL} 
	ZPROF=true $shell -i -c exit
}
pyenv () {
	local command=${1:-} 
	[ "$#" -gt 0 ] && shift
	case "$command" in
		(rehash | shell) eval "$(pyenv "sh-$command" "$@")" ;;
		(*) command pyenv "$command" "$@" ;;
	esac
}
relink () {
	src="$1" 
	dst="$2" 
	rm -rf "$dst"
	dir=$(dirname "$dst") 
	[[ ! -d "$dir" ]] && mkdir -p "$dir"
	ln -s "$src" "$dst"
}
rmd () {
	pandoc $1 | lynx -stdin
}
slit () {
	awk "{ print ${(j:,:):-\$${^@}} }"
}
sloc () {
	git ls-files | grep "$1" | grep -v grep | xargs wc -l
}
stream () {
	streamlink --player mpv --player-args '--ontop' $1 best
}
timevim () {
	for i in $(seq 1 10)
	do
		/usr/bin/time vim +qa
	done
}
timezsh () {
	shell=${1-$SHELL} 
	for i in $(seq 1 10)
	do
		time $shell -i -c exit
	done
}
tssh () {
	ssh $1 -t 'tmux has-session && tmux attach -t dhruva || tmux new -s dhruva'
}
# Shell Options
setopt autocd
setopt autopushd
setopt extendedhistory
setopt nohashdirs
setopt histexpiredupsfirst
setopt histignoredups
setopt histignorespace
setopt histverify
setopt incappendhistory
setopt login
setopt promptsubst
setopt sharehistory
# Aliases
alias -- bat='bat --plain'
alias -- claude-mem='bun "/Users/dhruva/.claude/plugins/cache/thedotmack/claude-mem/10.5.5/scripts/worker-service.cjs"'
alias -- d=docker
alias -- da='docker attach --detach-keys="ctrl-c"'
alias -- dc=docker-compose
alias -- diffdir='diff -rq'
alias -- docker=nerdctl
alias -- docker-compose='nerdctl compose'
alias -- get='curl --continue-at - --location --progress-bar --remote-name --remote-time'
alias -- grep='grep --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn} --exclude-dir={.bundle,node_modules}'
alias -- http-serve='python3 -m http.server'
alias -- k=kubectl
alias -- l='ls --color=auto -1tA'
alias -- lb='ledger bal -R'
alias -- lbn='ledger -R -X $ --current bal assets liabilities'
alias -- ledger='ledger -f ~/Dropbox/Documents/ledger/personal.dat'
alias -- ll='ls -lth --color=auto'
alias -- ls='ls --color=auto'
alias -- o=open
alias -- pbc=pbcopy
alias -- pbp=pbpaste
alias -- run-help=man
alias -- vimgit='git -c pager.color=false -c pager.log='\''vim -R -'\'' -c pager.show='\''vim -R -'\'' -c pager.diff='\''vim -R -'\'
alias -- which-command=whence
alias -- yt='youtube-dl --add-metadata -ic'
alias -- yta='youtube-dl --add-metadata -xic'
# Check for rg availability
if ! (unalias rg 2>/dev/null; command -v rg) >/dev/null 2>&1; then
  function rg {
  local _cc_bin="${CLAUDE_CODE_EXECPATH:-}"
  [[ -x $_cc_bin ]] || _cc_bin=/Users/dhruva/.local/bin/claude
  if [[ ! -x $_cc_bin ]]; then command rg "$@"; return; fi
  if [[ -n $ZSH_VERSION ]]; then
    ARGV0=rg "$_cc_bin" "$@"
  elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]] || [[ "$OSTYPE" == "win32" ]]; then
    ARGV0=rg "$_cc_bin" "$@"
  elif [[ $BASHPID != $$ ]]; then
    exec -a rg "$_cc_bin" "$@"
  else
    (exec -a rg "$_cc_bin" "$@")
  fi
}
fi
# Shadow find/grep with embedded bfs/ugrep
unalias find 2>/dev/null || true
unalias grep 2>/dev/null || true
function find {
  local _cc_bin="${CLAUDE_CODE_EXECPATH:-}"
  [[ -x $_cc_bin ]] || _cc_bin=/Users/dhruva/.local/bin/claude
  if [[ ! -x $_cc_bin ]]; then command find "$@"; return; fi
  if [[ -n $ZSH_VERSION ]]; then
    ARGV0=bfs "$_cc_bin" -S dfs -regextype findutils-default "$@"
  elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]] || [[ "$OSTYPE" == "win32" ]]; then
    ARGV0=bfs "$_cc_bin" -S dfs -regextype findutils-default "$@"
  elif [[ $BASHPID != $$ ]]; then
    exec -a bfs "$_cc_bin" -S dfs -regextype findutils-default "$@"
  else
    (exec -a bfs "$_cc_bin" -S dfs -regextype findutils-default "$@")
  fi
}
function grep {
  local _cc_a
  for _cc_a in "$@"; do
    case "$_cc_a" in -*-filter*|-*-pager*|-*-view*|-*-format-open*|-*-config*|---*|-@*|-*-save-config*) command grep "$@"; return ;; esac
  done
  local _cc_bin="${CLAUDE_CODE_EXECPATH:-}"
  [[ -x $_cc_bin ]] || _cc_bin=/Users/dhruva/.local/bin/claude
  if [[ ! -x $_cc_bin ]]; then command grep "$@"; return; fi
  if [[ -n $ZSH_VERSION ]]; then
    ARGV0=ugrep "$_cc_bin" -G --ignore-files --hidden -I --exclude-dir=.git --exclude-dir=.svn --exclude-dir=.hg --exclude-dir=.bzr --exclude-dir=.jj --exclude-dir=.sl "$@"
  elif [[ "$OSTYPE" == "msys" ]] || [[ "$OSTYPE" == "cygwin" ]] || [[ "$OSTYPE" == "win32" ]]; then
    ARGV0=ugrep "$_cc_bin" -G --ignore-files --hidden -I --exclude-dir=.git --exclude-dir=.svn --exclude-dir=.hg --exclude-dir=.bzr --exclude-dir=.jj --exclude-dir=.sl "$@"
  elif [[ $BASHPID != $$ ]]; then
    exec -a ugrep "$_cc_bin" -G --ignore-files --hidden -I --exclude-dir=.git --exclude-dir=.svn --exclude-dir=.hg --exclude-dir=.bzr --exclude-dir=.jj --exclude-dir=.sl "$@"
  else
    (exec -a ugrep "$_cc_bin" -G --ignore-files --hidden -I --exclude-dir=.git --exclude-dir=.svn --exclude-dir=.hg --exclude-dir=.bzr --exclude-dir=.jj --exclude-dir=.sl "$@")
  fi
}
export PATH=/Users/dhruva/.opencode/bin:/opt/homebrew/opt/python3/libexec/bin:/Users/dhruva/bin:/Users/dhruva/dotfiles/bin:/Users/dhruva/.local/bin:/Users/dhruva/.cargo/bin:/Users/dhruva/.ghcup/bin:/opt/homebrew/opt/llvm/bin:/opt/homebrew/opt/gnu-sed/libexec/gnubin:/opt/homebrew/bin:/opt/homebrew/sbin:/Users/dhruva/.pyenv/shims:/Users/dhruva/.cargo/bin:/Users/dhruva/src/go/bin:/usr/local/go/bin:/Users/dhruva/.asdf/shims:/Users/dhruva/.asdf/bin:/opt/homebrew/bin:/opt/homebrew/sbin:/usr/local/bin:/System/Cryptexes/App/usr/bin:/usr/bin:/bin:/usr/sbin:/sbin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin:/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin:/Library/Apple/usr/bin:/Library/TeX/texbin:/Users/dhruva/.cargo/bin:/Users/dhruva/.radicle/bin:/Users/dhruva/Library/Android/sdk/emulator:/Users/dhruva/Library/Android/sdk/tools:/Users/dhruva/Library/Android/sdk/tools/bin:/Users/dhruva/Library/Android/sdk/platform-tools:/Users/dhruva/.claude/plugins/cache/everything-claude-code/everything-claude-code/1.8.0/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/frontend-design/unknown/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/code-review/unknown/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/github/unknown/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/feature-dev/unknown/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/playwright/unknown/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/code-simplifier/1.0.0/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/superpowers/5.1.0/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/typescript-lsp/1.0.0/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/claude-md-management/1.0.0/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/figma/2.2.12/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/security-guidance/unknown/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/serena/unknown/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/pr-review-toolkit/unknown/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/skill-creator/unknown/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/pyright-lsp/1.0.0/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/plugin-dev/unknown/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/gopls-lsp/1.0.0/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/rust-analyzer-lsp/1.0.0/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/lua-lsp/1.0.0/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/semgrep/0.5.3/bin:/Users/dhruva/.claude/plugins/cache/thedotmack/claude-mem/10.5.5/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/context7/unknown/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/ralph-loop/1.0.0/bin:/Users/dhruva/src/oss/ruby-upgrade-toolkit/bin:/Users/dhruva/.claude/plugins/cache/claude-plugins-official/ruby-lsp/1.0.0/bin
