#!/bin/sh

if mpc status | grep -q "playing"; then
    echo "[playing]"
else
    echo ""

fi
