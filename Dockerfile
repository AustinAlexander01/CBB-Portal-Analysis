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
CMD ["/bin/sh", "-c", "Rscript -e \"library(DBI); library(RPostgres); h=Sys.getenv('SUPABASE_HOST','aws-1-us-east-2.pooler.supabase.com'); p=as.integer(Sys.getenv('SUPABASE_PORT','6543')); pw=Sys.getenv('SUPABASE_DB_PASSWORD'); cat('=== DB TEST ===\\nHost:',h,'\\nPort:',p,'\\nPW length:',nchar(pw),'\\n'); tryCatch({ con <- dbConnect(Postgres(), host=h, port=p, dbname='postgres', user='postgres.mkrllsjvjliyxgukwfme', password=pw, sslmode='prefer'); cat('DB CONNECTION OK\\n'); dbDisconnect(con) }, error=function(e) cat('DB CONNECTION FAILED:', conditionMessage(e), '\\n'))\" 2>&1 && exec /usr/bin/shiny-server"]
