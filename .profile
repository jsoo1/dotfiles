#! /run/current-system/profile/bin/env fish

set -l pidfile "$HOME/var/run/shepherd/pid"
set -l logfile "$HOME/var/log/shepherd.log"
set -l conf "$HOME/.config/shepherd/init.scm"
set -l shep_cmd  "shepherd --pid=$pidfile --socket=$MY_SHEP_SOCK --logfile=$logfile --config=$conf"

if status is-login
    if not herd --socket $MY_SHEP_SOCK status
        rm "$pidfile" "$MY_SHEP_SOCK"
        eval "$shep_cmd" 2>&1 >> "$logfile"
    end 2>/dev/null 1>/dev/null
end
