FROM rocker/shiny:4.4.2

# Install system dependencies required by R packages
RUN apt-get update && apt-get install -y --no-install-recommends \
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

# Install R packages: Bioconductor and CRAN dependencies
RUN R -e "install.packages('BiocManager', repos='https://cloud.r-project.org/')" && \
    R -e "BiocManager::install('flowCore', update = FALSE, ask = FALSE)" && \
    R -e "install.packages(c('shiny', 'bslib', 'ggplot2', 'dplyr', 'tidyr', 'readr', 'stringr', 'drc', 'scales', 'sp', 'zip', 'svglite', 'outliers', 'DescTools', 'pheatmap', 'gridExtra', 'rmarkdown', 'kableExtra', 'DT', 'ggrepel', 'knitr', 'htmltools'), repos='https://cloud.r-project.org/')"

# Set working directory
WORKDIR /srv/shiny-server

# Copy all files from the current directory into the container
# This includes app.R, generic_analysis.R, Rmd templates, and the www folder (if it exists)
COPY . ./

# Expose the port for Hugging Face
EXPOSE 7860

# Run the Shiny app
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host='0.0.0.0', port=7860)"]
