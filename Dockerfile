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
CMD ["/bin/sh", "-c", "Rscript -e \"library(httr2); jwt=nchar(Sys.getenv('SUPABASE_JWT','')); anon=nchar(Sys.getenv('SUPABASE_ANON_KEY','')); cat('=== REST TEST ===\\nSUPABASE_JWT length:',jwt,'SUPABASE_ANON_KEY length:',anon,'\\n'); key=if(jwt>0) Sys.getenv('SUPABASE_JWT') else Sys.getenv('SUPABASE_ANON_KEY'); tryCatch({ resp <- request('https://mkrllsjvjliyxgukwfme.supabase.co/rest/v1/basketball_players') |> req_headers(apikey=key, Authorization=paste('Bearer',key)) |> req_url_query(select='Name', limit=1) |> req_perform(); cat('REST CONNECTION OK\\n') }, error=function(e) cat('REST CONNECTION FAILED:', conditionMessage(e), '\\n'))\" 2>&1 && exec /usr/bin/shiny-server"]
