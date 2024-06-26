FROM rocker/shiny-verse:latest 
RUN apt-get update && apt-get install -y \
    git \
    libssl-dev \
    libcurl4-gnutls-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*


RUN git clone https://github.com/jamiekim1/optionsapp.git /srv/shiny-server/optionsapp


RUN R -e "install.packages('remotes')"

RUN R -e "install.packages('roptions', dependencies=TRUE, repos='http://cran.rstudio.com/')"


RUN R -e "remotes::install_deps('/srv/shiny-server/optionsapp')"


RUN R -e "remotes::install_local('/srv/shiny-server/optionsapp')"


EXPOSE 3939


CMD R -e "options('shiny.port'=3939, 'shiny.host'='0.0.0.0');optionsapp::run_app()"

