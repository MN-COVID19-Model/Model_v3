# get shiny server plus tidyverse packages image
FROM rocker/shiny

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev


# install R packages required
# (change it dependeing on the packages you need)
RUN R -e "install.packages(c('plyr', 'ggplot2', 'shinycssloaders', 'shiny', 'magrittr', 'munsell', 'colorspace', 'xtable', 'R6', 'rlang', 'fastmap', 'tools', 'grid', 'gtable', 'withr', 'htmltools', 'ellipsis', 'digest', 'tibble', 'lifecycle', 'crayon', 'later', 'vctrs', 'promises', 'glue', 'mime', 'compiler', 'pillar', 'scales', 'jsonlite', 'httpuv', 'pkgconfig'), repos='http://cran.rstudio.com/')"

# copy the app to the image
COPY R /srv/shiny-server/R
COPY data /srv/shiny-server/data

# select port
EXPOSE 3838

# allow permission
RUN sudo chown -R shiny:shiny /srv/shiny-server

# run app
WORKDIR /srv/shiny-server
CMD R -e "shiny::runApp('R', port = 3838, host = '0.0.0.0')"
