#! /usr/bin/env bash


case "$1" in
    connect) launchctl bootstrap gui/$(id -u) ~/Library/LaunchAgents/com.$2.de-tunnel.plist ;;
    disconnect) launchctl bootout gui/$(id -u)/com.$(~/vpn/status).de-tunnel ;;
esac

if ( ~/vpn/status ); then
    echo '---'
    echo "Disconnect | bash='$0' param1=disconnect terminal=false"
else
    echo '---'
    echo "VetPro | bash='$0' param1=connect param2=vetpro terminal=false"
    echo '---'
    echo "Pano | bash='$0' param1=connect param2=pano terminal=false"
fi
