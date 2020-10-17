#!/bin/bash

# works with this udev rule for me
# SUBSYSTEM=="drm", ACTION=="add", ENV{DISPLAY}=":0", RUN+="/bin/su nix -c '/home/nix/.local/hbv_scripts/udev_multimonitor.sh'"
# SUBSYSTEM=="drm", ACTION=="remove", ENV{DISPLAY}=":0", RUN+="/bin/su nix -c '/home/nix/.local/hbv_scripts/udev_multimonitor.sh'"

set -e

echo runnin >> /home/nix/test
if /usr/bin/xrandr | grep "DP-1-3" | grep "disconnected"; then
    /usr/bin/xrandr --output DP-1-3 --off
    /usr/bin/notify-send --urgency=low -t 5000 "Graphics Update" "External monitor disconnected"
else
    xrandr --output eDP-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output DP-1-3 --mode 1920x1080 --pos 1920x0 --rotate normal
    /usr/bin/notify-send --urgency=low -t 5000 "Graphics Update" "VGA plugged in"
fi

