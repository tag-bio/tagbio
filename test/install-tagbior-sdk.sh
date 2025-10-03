#!/usr/bin/env bash
set -ex


echo "
***************************************
  Installing Tag.bio Python & R SDKs
***       pip is $(which pip)       ***
***     conda is $(which conda)     ***
***      $(mamba --version)         ***
***************************************
"

# mamba bash hook
eval "$(mamba shell hook --shell bash)"

mamba env update -n base -f /environment.yml
mamba run -n base R CMD INSTALL /tagbio_latest.tgz