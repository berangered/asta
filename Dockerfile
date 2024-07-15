# Base image
FROM rocker/shiny:4.4.1

# Install required linux librairies
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends libpq-dev \ 
                                               libssl-dev \
                                               libxml2-dev \
                                               libglpk40

# Install R package and its dependencies
RUN install2.r devtools
COPY asta/ ./asta
RUN Rscript -e 'devtools::install_local(path = "./asta", dependencies = TRUE)'

# Expose port where shiny app will broadcast
ARG SHINY_PORT=3838
EXPOSE $SHINY_PORT
RUN echo "local({options(shiny.port = ${SHINY_PORT}, shiny.host = '0.0.0.0')})" >> /usr/local/lib/R/etc/Rprofile.site

# Endpoint
CMD ["Rscript", "-e", "library(asta) ; run_app()"]
