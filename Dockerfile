FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libpq-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages('renv', repos='https://cran.rstudio.com/')"

WORKDIR /app

COPY renv.lock renv.lock
COPY renv/ renv/

RUN R -e "renv::restore(project = '/app')"

COPY app.R app.R
COPY ui.R ui.R
COPY server.R server.R
COPY plotly_helpers.R plotly_helpers.R

COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
