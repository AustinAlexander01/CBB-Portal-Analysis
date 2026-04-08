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
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

EXPOSE 3838
CMD ["/bin/sh", "-c", "echo \"SUPABASE_ANON_KEY=${SUPABASE_ANON_KEY}\" > /home/shiny/.Renviron && echo \"PLAYER_STATS_CACHE_HOURS=${PLAYER_STATS_CACHE_HOURS}\" >> /home/shiny/.Renviron && exec /usr/bin/shiny-server"]
