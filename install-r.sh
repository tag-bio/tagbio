#!/usr/bin/env sh
set -e

export R_BASE_VERSION=${1:-4.0.2}
export R_MAJOR=$(echo $R_BASE_VERSION|cut -b 1)

conda config --add channels conda-forge
conda install \
  r-base=$R_BASE_VERSION \
  r-essentials \
  r-littler

ln -s /usr/lib/R/site-library/littler/examples/install.r /usr/local/bin/install.r
ln -s /usr/lib/R/site-library/littler/examples/install2.r /usr/local/bin/install2.r
ln -s /usr/lib/R/site-library/littler/examples/installGithub.r /usr/local/bin/installGithub.r
ln -s /usr/lib/R/site-library/littler/examples/testInstalled.r /usr/local/bin/testInstalled.r

conda clean --all -y
apt-get clean -y
apt-get autoremove -y
