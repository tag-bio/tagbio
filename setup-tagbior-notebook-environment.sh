#! /usr/bin/env bash
set -ex

mamba init bash
source /root/.bashrc

# CREATE TAGBIOPY-SPECIFIC NOTEBOOK
mamba create -n tagbior-notebook
mamba activate tagbior-notebook

# ADD R DEPENDENCIES
mamba env update -n $(echo $CONDA_DEFAULT_ENV) -f ${TAGBIO_R_UTILS}/environment.yml

# INSTALL PYTHON SDK
mamba run -n $(echo $CONDA_DEFAULT_ENV) R CMD INSTALL $TAGBIO_R_UTILS/tagbio_latest.tgz
