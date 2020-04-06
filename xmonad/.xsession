#! /run/current-system/profile/bin/env fish

xsetroot -cursor_name left_ptr
exec dbus-run-session -- xmonad
