#! /run/current-system/profile/bin/env fish

xsetroot -cursor_name left_ptr
xdg-settings set default-web-browser icecat.desktop
clipmenud &
pulseaudio -D
exec dbus-run-session -- xmonad
