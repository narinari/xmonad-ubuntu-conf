#!/usr/bin/env zsh
for port in VGA1 DP1 DP2 DP3 HDMI1 HDMI2 HDMI3; do
  xrandr | grep "$port disconnected" | ifne xrandr --output $port --off
  xrandr | grep "$port connected" | ifne xrandr --output $port --auto --right-of LVDS1
done
