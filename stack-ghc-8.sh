#!/bin/bash

# PPA repository: https://launchpad.net/~hvr/+archive/ubuntu/ghc
#
# apt-get install cabal-install-1.24 ghc-8.0.1 ghc-8.0.1-prof \
#   ghc-8.0.1-htmldocs ghc-8.0.1-dyn
export PATH="/opt/ghc/8.0.1/bin:/opt/cabal/1.24/bin:${PATH}"
root="$(dirname "$0")"

readonly stackBin='/usr/bin/stack'
readonly yamlFile="${root}/stack-nightly.yaml"

exec "${stackBin}" --stack-yaml "${yamlFile}" --skip-ghc-check "$@"
