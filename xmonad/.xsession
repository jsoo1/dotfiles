#! /run/current-system/profile/bin/env fish

xsetroot -cursor_name left_ptr
xdg-settings set default-web-browser icecat.desktop
clipmenud &
exec dbus-run-session -- xmonad
