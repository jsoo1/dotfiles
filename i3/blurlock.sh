#! /bin/bash

scrot /tmp/screenshot.png
convert /tmp/screenshot.png -blur 0x5 /tmp/screenshotblur.png
rm /tmp/screenshot.png
i3lock -i /tmp/screenshotblur.png
