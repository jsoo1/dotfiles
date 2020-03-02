#! /run/current-system/profile/bin/bash

xsetroot -cursor_name left_ptr
shepherd --socket=/home/john/var/run/shepherd/socket --config=/home/john/dotfiles/shepherd/init.scm --logfile=/home/john/var/log/shepherd.log --insecure
xmonad
