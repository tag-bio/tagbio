#!/usr/bin/env sh
set -e
set +x

# Remember to update links before checking in code!
# ln tagbio_1.1.44.tgz tagbio_latest.tgz 
# ln tagbio_1.1.44.tar.gz tagbio_latest.tar.gz 
# Retired:
# TAGBIO_R_VERSION=${1:-1.1.44}
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
  docopt \
  python=3.10 \
  r-argparse \
  r-broom \
  r-docopt \
  r-dplyr \
  r-gridextra
  r-httr \
  r-lifecycle \
  r-modelri \
  r-pillar \
  r-qpdf \
  r-rjson \
  r-svglite \
  r-tidyr \
  r-tidyverse \
  r-yaml \
  pandoc

  #bioconductor-gsva=1.42.0 
  #pip 

echo "Installing tagbio R package itself"
R CMD INSTALL $TAGBIO_R_UTILS/tagbio_latest.tgz

mamba clean --all -y
conda clean --all -y
apt-get clean -y
apt-get autoremove -y
