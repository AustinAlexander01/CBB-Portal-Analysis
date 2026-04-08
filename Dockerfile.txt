FROM rocker/shiny:latest
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    libxml2-dev \
    libglpk-dev \
    cmake \
    libicu-dev \
    curl \
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
COPY .Renviron /home/shiny/.Renviron
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
RUN R -e "source('server.R'); saveRDS(compute_app_data(), '.player_stats_cache.rds')"
RUN echo "bust-cache-v2"
EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=3838)"]