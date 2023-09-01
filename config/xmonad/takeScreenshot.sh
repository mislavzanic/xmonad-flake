#!/usr/bin/env bash

set -euo pipefail

# Specifying a directory to save our screenshots.
SCROTDIR="$HOME/.local/screenshots"
# Makes sure the directory exists.
mkdir -p "${SCROTDIR}"

getStamp() {
  date '+%Y%m%d-%H%M%S'
}

MAIM_ARGS=""
FILE_TYPE=""

# Get monitors and their settings for maim
DISPLAYS=$(xrandr --listactivemonitors | grep '+' | awk '{print $4, $3}' | awk -F'[x/+* ]' '{print $1,$2"x"$4"+"$6"+"$7}')

# What modes do we have
declare -a modes=(
"Fullscreen"
"Active window"
"Selected region"
)

# Add monitor data
IFS=$'\n'
declare -A DISPLAY_MODE
for i in ${DISPLAYS}; do
  name=$(echo "${i}" | awk '{print $1}')
  rest="$(echo "${i}" | awk '{print $2}')"
  modes[${#modes[@]}]="${name}"
  DISPLAY_MODE[${name}]="${rest}"
done
unset IFS

target="$@"
case "$target" in
  'Fullscreen')
    FILE_TYPE="full"
  ;;
  'Active Window')
    active_window=$(xdotool getactivewindow)
    MAIM_ARGS="-i ${active_window}"
    FILE_TYPE="window"
  ;;
  'Region')
    MAIM_ARGS="-s"
    FILE_TYPE="region"
  ;;
  *)
    MAIM_ARGS="-g ${DISPLAY_MODE[${target}]}"
    FILE_TYPE="${target}"
  ;;
esac

maim ${MAIM_ARGS} | tee "${SCROTDIR}/scrot-${FILE_TYPE}-$(getStamp).png" | xclip -selection clipboard -t image/png

exit 0
