FROM jupyter/datascience-notebook:lab-2.2.9

ENV DEBIAN_FRONTEND noninteractive

USER root

# update apt & conda base packages
RUN apt-get update -y && apt-get upgrade -y
RUN conda update --all -y

# Installing R component
ENV TAGBIO_R_UTILS /tagbior
COPY . /tagbior
RUN /tagbior/install.sh

# run test script
SHELL ["/opt/conda/condabin/conda", "run", "-n", "base", "/bin/bash", "-c"]
RUN Rscript /tagbior/test/test-in-docker.R

USER jovyan

