#!/bin/bash
# Claude Code status line, derived from the zsh PROMPT in
# ~/dotfiles/zsh/pristine_prompt:
#
#   %F{blue}%n%f@%F{blue}%M%f in %F{green}%~%f$(git-info)
#   %F{yellow}λ%f
#
# git-info appends " on <branch> <status-glyph>" (✔ clean / ✗ dirty / ? has
# untracked files), same as git-info/git-change-summary in that file.
# Folded in after the λ: current model + context-window usage, since those
# are the Claude-Code-specific facts the shell prompt has no equivalent of.

input=$(cat)

model=$(echo "$input" | jq -r '.model.display_name')
cwd=$(echo "$input" | jq -r '.workspace.current_dir // .cwd')
pct=$(echo "$input" | jq -r '.context_window.used_percentage // empty')

RESET=$'\033[0m'
DIM=$'\033[2m'
BLUE=$'\033[34m'
GREEN=$'\033[32m'
MAGENTA=$'\033[35m'
CYAN=$'\033[36m'
RED=$'\033[31m'
YELLOW=$'\033[33m'
GRAY=$'\033[90m'

# %~ : abbreviate $HOME to ~
display_cwd="${cwd/#$HOME/~}"

# git-info / git-change-summary: " on <branch> <glyph>"
git_info=""
if git -C "$cwd" --no-optional-locks rev-parse >/dev/null 2>&1; then
  branch=$(git -C "$cwd" --no-optional-locks symbolic-ref --short HEAD 2>/dev/null)
  [ -z "$branch" ] && branch=$(git -C "$cwd" --no-optional-locks rev-parse --short HEAD 2>/dev/null)
  if [ -n "$branch" ]; then
    first_change=$(git -C "$cwd" --no-optional-locks status --porcelain 2>/dev/null | head -1)
    if [ -z "$first_change" ]; then
      glyph="${DIM}${GREEN}✔${RESET}"
    elif [ "${first_change:0:1}" = "?" ]; then
      glyph="${DIM}${CYAN}?${RESET}"
    else
      glyph="${DIM}${RED}✗${RESET}"
    fi
    git_info=" on ${DIM}${MAGENTA}${branch}${RESET} ${glyph}"
  fi
fi

# Context usage, colored by severity
ctx_segment=""
if [ -n "$pct" ]; then
  ctx_int=${pct%.*}
  ctx="$(printf '%.0f' "$pct")%"
  if [ "$ctx_int" -ge 80 ]; then
    ctx_color="$RED"
  elif [ "$ctx_int" -ge 50 ]; then
    ctx_color="$YELLOW"
  else
    ctx_color="$GREEN"
  fi
  ctx_segment=" ${DIM}${GRAY}|${RESET} ${DIM}${ctx_color}${ctx}${RESET}"
fi

line="${DIM}${GREEN}${display_cwd}${RESET}${git_info} ${DIM}${YELLOW}λ${RESET} ${DIM}${CYAN}${model}${RESET}${ctx_segment}"

printf '%s' "$line"
