#!/usr/bin/env bash

# if [ "$1" = "$FOCUSED_WORKSPACE" ]; then
#     sketchybar --set $NAME background.drawing=on
# else
#     sketchybar --set $NAME background.drawing=off
# fi

sketchybar --set $NAME label="${FOCUSED_WORKSPACE-1}"
