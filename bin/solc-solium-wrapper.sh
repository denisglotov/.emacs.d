#!/bin/sh
#
# Wrapper for running both solc and solium from flycheck-solidity.
#

#echo $1 >>/tmp/solc-solium-wrapper.log
set -e
solc $1 github.com=..
cd $(dirname $1) && solium -R gcc -c .soliumrc.json -f $(basename $1)
