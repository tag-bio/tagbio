#!/usr/bin/env sh
set -e

# package dependencies
echo "Installing tagbio R system dependencies"
apt-get update
apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  python3

echo "Installing tagbio R package dependencies"
install.r argparse gridExtra httr qpdf rjson tidyverse

echo "Installing tagbio R package itself"
install.r $TAGBIO_R_UTILS/tagbio_0.2.0.tar.gz

