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
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY .Renviron /home/shiny/.Renviron
ARG SUPABASE_DB_PASSWORD
ARG SUPABASE_HOST
ARG SUPABASE_USER
ENV SUPABASE_DB_PASSWORD=$SUPABASE_DB_PASSWORD
ENV SUPABASE_HOST=$SUPABASE_HOST
ENV SUPABASE_USER=$SUPABASE_USER
RUN rm -f .player_stats_cache.rds .player_stats_cache_meta.rds
RUN R -e "tryCatch({ source('server.R'); saveRDS(compute_app_data(), '.player_stats_cache.rds') }, error = function(e) { message('Cache pre-bake failed: ', e$message); quit(status=0) })"
RUN echo "bust-cache-v5"
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]