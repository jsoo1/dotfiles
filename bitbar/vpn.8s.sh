#! /usr/bin/env bash


case "$1" in
    connect) launchctl start com.pano.de-tunnel ;;
    disconnect) launchctl kill -15 com.pano.de-tunnel ;;
esac

if ( ~/vpn/status ); then
    echo '---'
    echo "Disconnect | bash='$0' param1=disconnect terminal=false"
else
    echo '---'
    echo "Connect | bash='$0' param1=connect terminal=false"
fi
