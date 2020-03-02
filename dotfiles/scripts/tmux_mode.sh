#!/bin/bash
if [ $( xprop -id $(xprop -root 32x '\t$0' _NET_ACTIVE_WINDOW | cut -f 2) -notype _NET_WM_NAME | cut -d " "  -f 3) = '"tmux"' ]; then
        i3-msg "mode tmux"
else
        i3-msg "mode default"
fi
