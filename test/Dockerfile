# syntax=docker/dockerfile:1

# Available versions of Jupyter Hub
# https://hub.docker.com/r/jupyter/datascience-notebook/tags?page=1&name=lab
ARG JUPYTER_IMAGE_VERSION=3.4.3

# Available versions of Eclipse Temurin:
# https://api.adoptium.net/v3/info/available_releases
ARG JAVA_VERSION=21
#############################################################################

FROM jupyter/datascience-notebook:lab-${JUPYTER_IMAGE_VERSION}

ARG JAVA_VERSION

ENV JAVA_VERSION=${JAVA_VERSION}
ENV JAVA_HOME=/usr/lib/jvm/temurin-${JAVA_VERSION}-jre-amd64
ENV PATH=$PATH:$JAVA_HOME/bin
ENV TAGBIO_R_UTILS=/tagbior
ENV TAGBIO_JARS=/tagbio-jars
ENV DEBIAN_FRONTEND=noninteractive
ENV DEBCONF_NOWARNINGS="yes"

COPY . /

USER root
SHELL ["/bin/bash", "-o", "pipefail", "-c"]

# Install system-level packages
# Install Tag.bio SDK \
# Run test script
RUN /test/install-system-packages.sh && \
  /test/install-java.sh && \
  /test/install-tagbio-jars.sh && \
  /test/install-conda-updates.sh && \
  /test/install-tagbior-sdk.sh && \
  Rscript /test/test-in-docker.R 2>&1

USER jovyan
