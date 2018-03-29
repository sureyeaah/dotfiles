#!/bin/sh
set -e
playerctl status 2>/dev/null | grep -q "Playing"
title="$(playerctl metadata xesam:title)"
artist="$(playerctl metadata xesam:artist)"
echo "$title - $artist"
