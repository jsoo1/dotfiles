export PATH="/home/john/.local/bin:$PATH"
export EDITOR="TERM=xterm-24bits emacsclient -nw --socket-name term"
export GUIX_LOCPATH="$HOME/.guix-profile/lib/locale"
export GST_PLUGIN_SYSTEM_PATH="/home/john/.guix-profile/lib/gstreamer-1.0${GST_PLUGIN_SYSTEM_PATH:+:}$GST_PLUGIN_SYSTEM_PATH"
export GIO_EXTRA_MODULES="/home/john/.guix-profile/lib/gio/modules${GIO_EXTRA_MODULES:+:}$GIO_EXTRA_MODULES"

eval "$(guix package --search-paths | grep -E 'export (GHC_PACKAGE_PATH|COQPATH|IDRIS_LIBRARY_PATH)=')"

# Honor per-interactive-shell startup file
if [ -f ~/.bashrc ]; then . ~/.bashrc; fi

