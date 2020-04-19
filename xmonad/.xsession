#! /run/current-system/profile/bin/env fish

xsetroot -cursor_name left_ptr
clipmenud &
exec dbus-run-session -- xmonad
