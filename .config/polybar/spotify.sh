#!/bin/sh
set -e

zscroll -l 30 \
        --delay 0.075 \
        -u true "/home/shaurya/.config/polybar/spotify-status.sh" &

wait
