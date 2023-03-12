#!/usr/bin/env sh
set -e
set +x

# Remember to update links before checking in code!
# ln tagbio_1.1.46.tgz tagbio_latest.tgz 
# ln tagbio_1.1.46.tar.gz tagbio_latest.tar.gz 
# Retired:
# TAGBIO_R_VERSION=${1:-1.1.46}
# echo "Installing tagbio R version $TAGBIO_R_VERSION"

# package dependencies
echo "Updating apt cache"
apt-get update
echo "Installing tagbio R system dependencies"
echo "Installing libcario2"
#apt-get install -y \
apt-get install --no-install-recommends -y  libcairo2-dev 
echo "Installing libcurl4"
apt-get install --no-install-recommends -y  libcurl4-openssl-dev 
echo "Installing libfont"
apt-get install --no-install-recommends -y  libfontconfig1-dev 
echo "Installing libssl"
apt-get install --no-install-recommends -y  libssl-dev 
echo "Installing libxml"
apt-get install --no-install-recommends -y  libxml2-dev 
echo "Installing libxt"
apt-get install --no-install-recommends -y  libxt-dev 
echo "Installing apt-utils"
apt-get install --no-install-recommends -y  apt-utils

echo "Adding conda channels"
conda config --add channels bioconda
conda config --add channels conda-forge
# conda update --all -y
echo "Installing packages with conda"
conda install -y -c bioconda -c conda-forge \
  docopt=0.6.2 \
  pip \
  python=3.10 \
  r-argparse=2.1.6 \
  r-broom=1.0.1 \
  r-docopt=0.7.1 \
  r-dplyr=1.0.10 \
  r-gridextra=2.3 \
  r-httr=1.4.4 \
  r-lifecycle=1.0.3 \
  r-modelr=0.1.9 \
  r-pillar=1.8.1 \
  r-qpdf=1.3.0 \
  r-rjson=0.2.21 \
  r-svglite=2.1.0 \
  r-tidyr=1.2.1 \
  r-tidyverse=1.3.2 \
  r-yaml=2.3.6 \
  pandoc=2.19.2

echo "Installing tagbio R package itself"
R CMD INSTALL $TAGBIO_R_UTILS/tagbio_latest.tgz

mamba clean --all -y
conda clean --all -y
apt-get clean -y
apt-get autoremove -y
