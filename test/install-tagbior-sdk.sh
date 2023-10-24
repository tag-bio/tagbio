#!/usr/bin/env bash
set -ex


echo "
************************
Installing Tag.bio R SDK
***     pip is $(which pip)    ***
***     conda is $(which conda)    ***
************************
"
mamba env update -f ${TAGBIO_R_UTILS}/environment.yml
mamba run -n base ${TAGBIO_R_UTILS}/install-package.sh