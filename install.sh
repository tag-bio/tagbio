#!/usr/bin/env sh
set -e
set -x

TAGBIO_R_VERSION=${1:-0.9.0}
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
  python3

echo "Installing tagbio R package dependencies"
# do them one at a time for early failure
install2.r -e argparse
install2.r -e gridExtra
install2.r -e httr
install2.r -e qpdf
install2.r -e rjson
install2.r -e broom
install2.r -e tidyr
install2.r -e modelr
install2.r -e tidyverse
install2.r -e svglite

echo "Installing tagbio R package itself"
install2.r -e $TAGBIO_R_UTILS/tagbio_$TAGBIO_R_VERSION.tgz

