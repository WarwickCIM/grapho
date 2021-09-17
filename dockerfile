# container for grapho development
# we run rstudio server in a standardised environment
FROM rocker/rstudio:4.0.5

MAINTAINER James Tripp <james.tripp@warwick.ac.uk>

RUN apt-get clean all && \
  apt-get update && \
  apt-get upgrade -y && \
  apt-get install -y \
    libhdf5-dev \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    libxt-dev \
    zlib1g-dev \
    libbz2-dev \
    liblzma-dev \
    libglpk40 \
    libgit2-28 \
    r-cran-devtools \
    texlive-latex-recommended \
  && apt-get clean all && \
  apt-get purge && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN Rscript -e "install.packages(c('rmarkdown', 'rstudioapi', 'knitr', 'testthat', 'digest', 'devtools'));"

# Build the container by running, at the command line
## docker build -t grapho/rstudio-server .
# Then launch the container with the below.
# The container will have access to the current working directory.
# The username is rstudio and the password is password
# The rstudio interface is available at 127.0.0.1:8888
# The local ssh key folder is mapped.
# Please make sure you have an ~/.ssh directory
# docker run --rm \
#           -p 8888:8787 \
#           -d \
#           --name rstudio_server \
#           -v $(pwd):/home/rstudio \
#           -e PASSWORD=password \
#           -e USERID=$(id -u) \
#           -e GROUPID=$(id -g) \
#           grapho/rstudio-server