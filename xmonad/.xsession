#! /run/current-system/profile/bin/env fish

set -Xg DBUS_SESSION_BUS_ADDRESS (dbus-run-session)
xsetroot -cursor_name left_ptr
exec xmonad
