#!/bin/bash
cat >/tmp/screen-24bit-256color.terminfo <<EOF
screen-24bit-256color|xterm with 24-bit direct color mode,
  use=screen-256color,
  sitm=\E[3m,
  ritm=\E[23m,
  setb24=\E[48;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,
  setf24=\E[38;2;%p1%{65536}%/%d;%p1%{256}%/%{255}%&%d;%p1%{255}%&%dm,

EOF
tic -x -o ~/.terminfo /tmp/screen-24bit-256color.terminfo
