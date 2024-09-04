#!/usr/bin/env sh
set -ex

echo "Installing tagbio R package itself"
mamba run -n $(echo $CONDA_DEFAULT_ENV) R CMD INSTALL $TAGBIO_R_UTILS/tagbio_latest.tgz