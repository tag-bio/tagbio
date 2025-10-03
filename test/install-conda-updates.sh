#!/usr/bin/env bash
set -ex


# Remove any pinned package lists
# rm -rf "$(conda info --base)"
rm /opt/conda/conda-meta/pinned


# For r-base, libjpeg_turbo package conflicts with jpeg.
# jpeg seems to also be the lynchpin for many upgrade path
# conflicts across R and Python.
mamba remove -y -n base \
  jpeg \
  nbclassic \
  widgetsnbextension \
  > /dev/null

conda update -y -n base --no-pin \
  conda \
  h5py \
  hdf5 \
  jupyterhub \
  jupyterhub-base \
  krb5 \
  libcurl \
  libedit \
  libmamba \
  libmambapy \
  mamba \
  pycurl \
  pytables \
  unixodbc \
  > /dev/null

# Update Conda, because this JupyterHub base image is very old.
mamba update -y -q -n base --update-deps --no-pin \
  "conda>=25" \
  mamba \
  pip \
  python \
  > /dev/null


# The base image has many packages in the
# base environment, which need to also be updated.
# conda update --all -y
# unfortunately wont work.
mamba update -y -q -n base \
  altair \
  beautifulsoup4 \
  bokeh \
  bottleneck \
  cloudpickle \
  cython \
  dask \
  dill \
  h5py \
  ipykernel \
  ipympl \
  ipywidgets \
  jinja2 \
  jupyterlab-git \
  matplotlib-base \
  notebook \
  numba \
  numexpr \
  openblas \
  openpyxl \
  pandas \
  patsy \
  protobuf \
  pytables \
  scikit-image \
  scikit-learn \
  scipy \
  seaborn \
  sqlalchemy \
  statsmodels \
  sympy \
  xlrd \
  > /dev/null

mamba install -y -q -n base \
  conda-merge \
  conda-pack \
  dash \
  ez_setup \
  matplotlib \
  plotly \
  pyopenssl=24.0.0 \
  r-base \
  r-caret \
  r-crayon \
  r-devtools \
  r-e1071 \
  r-forecast \
  r-hexbin \
  r-htmltools \
  r-htmlwidgets \
  r-irkernel \
  r-nycflights13 \
  r-randomforest \
  r-rcurl \
  r-repr \
  r-rmarkdown \
  r-rodbc \
  r-rsqlite \
  r-shiny \
  r-tidymodels \
  r-tidyverse \
  rpy2 \
  wheel \
  > /dev/null
