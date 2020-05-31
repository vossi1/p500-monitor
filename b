#!/bin/sh
acme -v monitor.b
diff -s monitor.prg original/monitor-fix02.prg
cmp monitor.prg original/monitor-fix02.prg