#!/usr/bin/env sh
set -e

export R_BASE_VERSION=${1:-4.0.2}
export R_MAJOR=${R_BASE_VERSION:0:1}

echo "Installing R $R_BASE_VERSION"

apt-get update -y
apt-get install -y \
  ed \
  ca-certificates \
  fonts-texgyre \
  gnupg \
  gnupg2 \
  less \
  locales \
  vim-tiny \
  wget

echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen
locale-gen en_US.utf8
/usr/sbin/update-locale LANG=en_US.UTF-8

export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8

apt-key adv --keyserver keys.gnupg.net --recv-key 'E19F5F87128899B192B1A2C2AD5F960A256A04AF'
if [ "$R_MAJOR" == "4" ]; then
  echo "deb http://cloud.r-project.org/bin/linux/debian buster-cran40/" >> /etc/apt/sources.list
elif [ "$R_MAJOR" == "3" ]; then
  echo "deb http://cloud.r-project.org/bin/linux/debian buster-cran35/" >> /etc/apt/sources.list
else
  echo "install-r.sh only supports major versions 3 and 4"
  exit 1
fi

# echo "deb http://http.debian.net/debian sid main" > /etc/apt/sources.list.d/debian-unstable.list
# echo 'APT::Default-Release "testing";' > /etc/apt/apt.conf.d/default

apt-get update
apt-get install -y \
  littler \
  r-cran-littler \
  r-base=${R_BASE_VERSION}-* \
  r-base-dev=${R_BASE_VERSION}-* \
  r-recommended=${R_BASE_VERSION}-*

ln -s /usr/lib/R/site-library/littler/examples/install.r /usr/local/bin/install.r
ln -s /usr/lib/R/site-library/littler/examples/install2.r /usr/local/bin/install2.r
ln -s /usr/lib/R/site-library/littler/examples/installGithub.r /usr/local/bin/installGithub.r
ln -s /usr/lib/R/site-library/littler/examples/testInstalled.r /usr/local/bin/testInstalled.r
install.r docopt
