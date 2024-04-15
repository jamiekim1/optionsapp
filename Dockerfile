FROM rocker/shiny-verse:latest 
RUN apt-get update && apt-get install -y git \
    libssl-dev \
    libcurl4-gnutls-dev

RUN git clone https://github.com/jamiekim1/optionsapp.git /srv/shiny-server/optionsapp
RUN R -e "remotes::install_deps('/srv/shiny-server/optionsapp')"

EXPOSE 3939

CMD ["R", "-e", "optionsapp::run_app()"]
