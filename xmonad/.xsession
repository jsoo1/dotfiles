#! /run/current-system/profile/bin/bash

xsetroot -cursor_name left_ptr
pulseaudio --daemonize
light -S 50.0
xmonad
