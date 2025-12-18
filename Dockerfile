FROM rocker/shiny:4.4.2

RUN apt-get update && apt-get install -y \
    pandoc \
    zip \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgmp-dev \
    libgsl-dev \
    libmpfr-dev \
    gfortran \
    liblapack-dev \
    libblas-dev \
    libopenblas-dev \
    zlib1g-dev \
    libfftw3-dev \
    libpng-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    libfreetype6-dev \
    libnlopt-dev \
    libmagick++-dev \
    && rm -rf /var/lib/apt/lists/*


COPY app.R facs_report.Rmd /srv/shiny-server/
WORKDIR /srv/shiny-server

EXPOSE 7860
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', host='0.0.0.0', port=7860)"]