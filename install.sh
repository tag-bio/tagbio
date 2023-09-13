#!/usr/bin/env sh
set -e
set +x

# Remember to update links before checking in code!
# ln tagbio_1.1.49.tgz tagbio_latest.tgz 
# ln tagbio_1.1.49.tar.gz tagbio_latest.tar.gz 
# Retired:
# TAGBIO_R_VERSION=${1:-1.1.49}
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

mamba update --all -y

echo "Installing packages with mamba"
mamba install -y -c conda-forge \
  docopt=0.6.2 \
  pandoc=2.19.2 \
  r-optparse=1.7.3 \
  r-httr=1.4.4 \
  r-qpdf=1.3.0 \
  r-rjson=0.2.21 \
  r-tidyverse=1.3.2 \
  r-yaml=2.3.6 \
  r-rmarkdown=2.24 \
  r-knitr=1.40
  
echo "Installing tagbio R package itself"
R CMD INSTALL $TAGBIO_R_UTILS/tagbio_latest.tgz

mamba clean --all -y
apt-get clean -y
apt-get autoremove -y
