# syntax=docker/dockerfile:1
ARG JUPYTER_IMAGE_VERSION=lab-3.4.3

FROM jupyter/datascience-notebook:${JUPYTER_IMAGE_VERSION}

ENV TAGBIO_R_UTILS /tagbior
ENV DEBIAN_FRONTEND noninteractive

COPY . $TAGBIO_R_UTILS

USER root
SHELL ["/usr/bin/bash", "-c"]

# Install system-level packages
# Install Tag.bio SDK \
# Run test script
RUN install-system-packages.sh && \
  install-tagbior-sdk.sh && \
  Rscript $TAGBIO_R_UTILS/test/test-in-docker.R

USER jovyan
