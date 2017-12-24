#!/bin/sh
#
# Wrapper for running both solc and solium from flycheck-solidity.
#
solc $1
cd $(dirname $1)
solium -R gcc -f $(basename $1)
