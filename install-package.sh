#!/usr/bin/env sh
set -ex

echo "Installing tagbio R package itself"
R CMD INSTALL $TAGBIO_R_UTILS/tagbio_latest.tgz