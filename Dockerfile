# Start with the official RStudio image
FROM rocker/rstudio:4.4.1

# Avoid user interaction with tzdata
ENV DEBIAN_FRONTEND=noninteractive

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    curl \
    libcurl4-gnutls-dev \
    libxml2-dev \
    libssl-dev \
    libfontconfig1-dev \
    libcairo2-dev \
    libxt-dev \
    xorg-dev \
    libreadline-dev \
    libbz2-dev \
    liblzma-dev \
    zlib1g-dev \
    gfortran \
    software-properties-common \
    bash \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libpng-dev \
    pkg-config \
    libtiff5-dev \
    libjpeg-dev \
    cmake \
    && rm -rf /var/lib/apt/lists/*

# Detect architecture and download the appropriate Miniconda installer
RUN ARCH=$(uname -m) && \
    if [ "$ARCH" = "x86_64" ]; then \
        curl -O https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh; \
    elif [ "$ARCH" = "aarch64" ]; then \
        curl -O https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-aarch64.sh; \
    else \
        echo "Unsupported architecture: $ARCH" && exit 1; \
    fi && \
    bash Miniconda3-latest-Linux-*.sh -b -p /opt/conda && \
    rm Miniconda3-latest-Linux-*.sh

# Add Conda to the PATH and initialize Conda globally for all users
ENV PATH="/opt/conda/bin:$PATH"
RUN /opt/conda/bin/conda init bash && \
    echo ". /opt/conda/etc/profile.d/conda.sh" > /etc/profile.d/conda.sh

# Install Python and its libraries
RUN /opt/conda/bin/conda install -y python=3.12.4
RUN /opt/conda/bin/conda install -y jupyterlab=4.0.11
RUN /opt/conda/bin/conda install -y pandas=2.2.2
RUN /opt/conda/bin/conda install -y scikit-learn=1.5.1

# Install R packages
RUN R -e "install.packages('BiocManager', repos='http://cran.rstudio.com/')"
RUN R -e "BiocManager::install('tidyverse', ask=FALSE, update=FALSE, force=TRUE)"
RUN R -e "BiocManager::install('kableExtra', ask=FALSE, update=FALSE, force=TRUE)"
RUN R -e "BiocManager::install('ggpubr', ask=FALSE, update=FALSE, force=TRUE)"
RUN R -e "BiocManager::install('dslabs', ask=FALSE, update=FALSE, force=TRUE)"

# Set the working directory to ~/project on R session start
RUN echo 'setwd("~/project")' >> /home/rstudio/.Rprofile

# Reset DEBIAN_FRONTEND variable
ENV DEBIAN_FRONTEND=

# Set the working directory
WORKDIR /home/rstudio/

# Expose ports for RStudio and JupyterLab
EXPOSE 8787 8888

# Set up the password for rstudio user
ENV PASSWORD=1234
RUN echo "rstudio:${PASSWORD}" | chpasswd && adduser rstudio sudo

# Start RStudio Server
CMD ["/init"]