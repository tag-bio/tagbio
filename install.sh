#!/usr/bin/env sh
set -e

install.r httr argparse qpdf rjson
install.r $TAGBIO_R_UTILS/tagbio_0.2.0.tar.gz

