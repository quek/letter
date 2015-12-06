#! /bin/sh
#
### BEGIN INIT INFO
# Provides:          letter
# Required-Start:    $remote_fs $syslog
# Required-Stop:     $remote_fs $syslog
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: Example initscript
# Description:       This file should be used to construct scripts to be
#                    placed in /etc/init.d.  This example start a
#                    single forking daemon capable of writing a pid
#                    file.  To get other behavoirs, implemend
#                    do_start(), do_stop() or other functions to
#                    override the defaults in /lib/init/init-d-script.
### END INIT INFO

. /lib/lsb/init-functions

PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
DAEMON=/home/ancient/quicklisp/local-projects/letter/bin/letter.sbcl
NAME=letter
DESC="letter"

test -x $DAEMON || exit 0

USER=ancient
GROUP=ancient

# Get lsb functions
. /lib/lsb/init-functions

set -e

do_start() {
    start-stop-daemon --start --background --quiet --pidfile /var/run/$NAME.pid -c $USER:$GROUP --exec $DAEMON
}

do_stop() {
    start-stop-daemon --stop --oknodo --quiet --pidfile /var/run/$NAME.pid --exec $DAEMON
}

case "$1" in
    start)
        log_daemon_msg "Starting $DESC" "$NAME"
        do_start
        log_end_msg $?
        ;;
    stop)
        log_daemon_msg "Stopping $DESC" "$NAME"
        do_stop
        log_end_msg $?
        ;;
    restart)
        log_daemon_msg "Restarting $DESC" "$NAME"
        do_stop
        do_start
        log_end_msg $?
        ;;
    *)
        N=/etc/init.d/$NAME
        log_failure_msg "Usage: $N {start|stop|restart}"
        exit 1
        ;;
esac

exit 0
