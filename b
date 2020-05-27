#!/bin/sh
acme -v monitor.b
diff -s monitor.prg original/monitor-e000.prg
cmp monitor.prg original/monitor-e000.prg