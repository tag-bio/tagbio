#!/usr/bin/env sh
set -e

# package dependencies
apt-get update
apt-get install -y \
  libcurl4-openssl-dev \
  libssl-dev \
  python3

install.r argparse gridExtra httr qpdf rjson
install.r $TAGBIO_R_UTILS/tagbio_0.2.0.tar.gz

