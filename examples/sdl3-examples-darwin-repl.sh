#!/bin/sh

BASE=`dirname $0`
open -W --stdin $(tty) --stdout $(tty) --stderr $(tty) ${BASE}/sdl3-repl.app
