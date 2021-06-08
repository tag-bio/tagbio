#!/usr/bin/env sh
set -e
set +x

TAGBIO_R_VERSION=${1:-1.1.8}
echo "Installing tagbio R version $TAGBIO_R_VERSION"

# package dependencies
echo "Installing tagbio R system dependencies"
apt-get update
apt-get install -y \
  libcairo2-dev \
  libcurl4-openssl-dev \
  libfontconfig1-dev \
  libssl-dev \
  libxml2-dev \
  libxt-dev

conda config --add channels bioconda
conda config --add channels conda-forge
# conda update --all -y
conda install \
  bioconductor-gsva \
  docopt \
  pip \
  python=3.8 \
  r-argparse \
  r-broom \
  r-docopt \
  r-dplyr \
  r-gridextra \
  r-httr \
  r-lifecycle \
  r-modelr \
  r-pillar \
  r-qpdf \
  r-rjson \
  r-svglite \
  r-tidyr \
  r-tidyverse 

echo "Installing tagbio R package itself"
R CMD INSTALL $TAGBIO_R_UTILS/tagbio_$TAGBIO_R_VERSION.tgz

conda clean --all -y
apt-get clean -y
apt-get autoremove -y
