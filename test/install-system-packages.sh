#!/usr/bin/env bash
set -ex

apt-get update -q > /dev/null
apt-get upgrade -y -q > /dev/null

echo "
*********************************
Install Compiler and System Stuff
*********************************
"
apt-get install -y -q --no-install-recommends \
  apt-transport-https \
  apt-utils \
  bash-completion \
  bzip2 \
  ca-certificates \
  cm-super \
  curl \
  dvipng \
  git \
  gnupg \
  gpg \
  groff \
  gzip \
  less \
  lsb-release \
  nano \
  pandoc \
  procps \
  ripgrep \
  rsync \
  screen \
  texlive-fonts-recommended \
  texlive-latex-extra \
  texlive-latex-recommended \
  texlive-plain-generic \
  texlive-xetex \
  tzdata \
  unzip \
  vim \
  wget \
  xclip \
  zip \
  > /dev/null


echo "
******************************************
Update to GCC 11, Install yq, Install Rust
******************************************
"
apt-get install -y -q --no-install-recommends \
  build-essential \
  manpages-dev \
  software-properties-common \
  > /dev/null


# # Install yq
# wget https://github.com/mikefarah/yq/releases/latest/download/yq_linux_amd64 -qO /usr/local/bin/yq 
# chmod +x /usr/local/bin/yq


# This repository is no longer maintained,
# but has what we need for Ubuntu 20.04:Focal.
# 22.04:Jammy includes gcc-11, g++-11 in the
# standard channel.
add-apt-repository ppa:ubuntu-toolchain-r/test && apt-get update -q > /dev/null

apt-get install -y -q --no-install-recommends \
  gcc-11 \
  g++-11 \
  > /dev/null

apt-get install --only-upgrade \
  libstdc++6 \
  > /dev/null

# Set GCC-11 system defaults
update-alternatives --quiet --remove-all cpp
update-alternatives --quiet --install /usr/bin/gcc gcc /usr/bin/gcc-9 90 --slave /usr/bin/g++ g++ /usr/bin/g++-9 --slave /usr/bin/gcov gcov /usr/bin/gcov-9 --slave /usr/bin/gcc-ar gcc-ar /usr/bin/gcc-ar-9 --slave /usr/bin/gcc-ranlib gcc-ranlib /usr/bin/gcc-ranlib-9  --slave /usr/bin/cpp cpp /usr/bin/cpp-9
update-alternatives --quiet --install /usr/bin/gcc gcc /usr/bin/gcc-11 110 --slave /usr/bin/g++ g++ /usr/bin/g++-11 --slave /usr/bin/gcov gcov /usr/bin/gcov-11 --slave /usr/bin/gcc-ar gcc-ar /usr/bin/gcc-ar-11 --slave /usr/bin/gcc-ranlib gcc-ranlib /usr/bin/gcc-ranlib-11  --slave /usr/bin/cpp cpp /usr/bin/cpp-11


# Install Rust non-interactive
curl https://sh.rustup.rs -sSf | sh -s -- -y
export PATH="$HOME/.cargo/bin:$PATH"
