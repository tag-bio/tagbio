#!/usr/bin/env sh
set -ex


eval "$(mamba shell hook --shell bash)"


# INSTALL TAGBIOR SDK
mamba run -n "$(echo $CONDA_DEFAULT_ENV)" R CMD INSTALL "$TAGBIO_R_UTILS"/tagbio_latest.tgz