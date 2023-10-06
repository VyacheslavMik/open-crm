#!/bin/bash
su - postgres -c '/usr/lib/postgresql/11/bin/pg_ctl -o "-F -p 5460" -D /data -l logfile start' &
screen -D -m sbcl --load load.lisp --eval '(server.core::start)'
