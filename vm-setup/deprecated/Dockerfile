# Use the base image
FROM gcr.io/deeplearning-platform-release/workbench-container:latest

# Install dependencies for R
RUN apt-get update -qq && apt-get install --no-install-recommends -y \
    software-properties-common \
    dirmngr \
    gnupg2 \
    build-essential \
    gcc \
    g++ \
    gfortran \
    make \
    libblas-dev \
    liblapack-dev \
    libatlas-base-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libreadline-dev \
    libpcre3-dev \
    liblzma-dev \
    libbz2-dev \
    libzstd-dev \
    libpng-dev \
    libjpeg-dev \
    libtiff-dev \
    libcairo2-dev \
    libtcl8.6 \
    libtk8.6 \
    libgmp-dev \
    libmpfr-dev \
    zlib1g-dev \
    xz-utils

# Add the signing key for the CRAN repository
RUN wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc

# Add the CRAN repository
RUN add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"

# Install R 4.4.1
RUN apt-get update && apt-get install -y \
    r-base=4.4.1-1.2204.0

# Clean up
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

# Verify R installation
RUN R --version
