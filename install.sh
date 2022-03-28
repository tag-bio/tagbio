#!/usr/bin/env sh
set -e
set +x

TAGBIO_R_VERSION=${1:-1.1.30}
echo "Installing tagbio R version $TAGBIO_R_VERSION"

# package dependencies
echo "Updating apt cache"
apt-get update
echo "Installing tagbio R system dependencies"
echo "Installing libcario2"
#apt-get install -y \
apt-get install -y  libcairo2-dev 
echo "Installing libcurl4"
apt-get install -y  libcurl4-openssl-dev 
echo "Installing libfont"
apt-get install -y  libfontconfig1-dev 
echo "Installing libssl"
apt-get install -y  libssl-dev 
echo "Installing libxml"
apt-get install -y  libxml2-dev 
echo "Installing libxt"
apt-get install -y  libxt-dev 
echo "Installing apt-utils"
apt-get install -y  apt-utils

echo "Adding conda channels"
conda config --add channels bioconda
conda config --add channels conda-forge
# conda update --all -y
echo "Installing packages with mamba"
mamba install -y -c bioconda -c conda-forge \
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
  r-tidyverse \
  r-yaml \
  pandoc

echo "Installing tagbio R package itself"
R CMD INSTALL $TAGBIO_R_UTILS/tagbio_$TAGBIO_R_VERSION.tgz

conda clean --all -y
apt-get clean -y
apt-get autoremove -y
