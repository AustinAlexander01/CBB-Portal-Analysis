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
ARG SUPABASE_DB_PASSWORD
ARG SUPABASE_HOST
ARG SUPABASE_USER
ENV SUPABASE_DB_PASSWORD=$SUPABASE_DB_PASSWORD
ENV SUPABASE_HOST=$SUPABASE_HOST
ENV SUPABASE_USER=$SUPABASE_USER
RUN echo "SUPABASE_DB_PASSWORD=${SUPABASE_DB_PASSWORD}" >> /home/shiny/.Renviron && \
    echo "SUPABASE_HOST=${SUPABASE_HOST}" >> /home/shiny/.Renviron && \
    echo "SUPABASE_USER=${SUPABASE_USER}" >> /home/shiny/.Renviron
RUN echo "bust-cache-v9"
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
