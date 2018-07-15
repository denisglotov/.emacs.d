#!/bin/sh
#
# Wrapper for running both solc and solium from flycheck-solidity.
#

#echo $PWD: $@ >>/tmp/solc-solium-wrapper.log
set -e
solc --allow-paths ../node_modules/zeppelin-solidity/ \
     github.com=.. zeppelin-solidity=../node_modules/zeppelin-solidity $@
#cd $(dirname $1) && solium -R gcc -c .soliumrc.json -f $(basename $1)
