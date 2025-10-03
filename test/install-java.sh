#!/usr/bin/env bash
set -ex


echo "
****************************************
Install Temurin Java Runtime Environment
****************************************
"
#
# 15FEB23 - Updated from:
# https://adoptium.net/installation/linux/
#
wget -qO - https://packages.adoptium.net/artifactory/api/gpg/key/public | 
  gpg --dearmor | 
  tee /etc/apt/trusted.gpg.d/adoptium.gpg > /dev/null

echo "deb https://packages.adoptium.net/artifactory/deb $(awk -F= '/^VERSION_CODENAME/{print$2}' /etc/os-release) main" | 
  tee /etc/apt/sources.list.d/adoptium.list

apt-get update -y

apt-get install -y temurin-"${JAVA_VERSION}"-jre
