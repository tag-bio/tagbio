#! /usr/bin/env bash
set -ex


eval "$(mamba shell hook --shell bash)"

# CREATE TAGBIOR NOTEBOOK ENVIRONMENT
mamba create -n tagbior-notebook

# ADD R DEPENDENCIES
mamba env update -n tagbior-notebook -f "${TAGBIO_R_UTILS}"/environment.yml

# INSTALL TAGBIOR SDK
mamba run -n tagbior-notebook R CMD INSTALL "$TAGBIO_R_UTILS"/tagbio_latest.tgz

mamba activate tagbior-notebook
