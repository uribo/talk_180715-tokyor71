FROM rocker/geospatial:3.5.1

RUN set -x && \
  apt-get update && \
  apt-get install -y --no-install-recommends \
    fonts-ipaexfont && \
  apt-get clean && \
  localedef -f UTF-8 -i ja_JP ja_JP.UTF-8 && \
  rm -rf /var/lib/apt/lists/* && \
  install2.r --error \
    here \
    ggforce \
    ggrepel \
    jpmesh \
    jpndistrict && \
  installGithub.r \
    'tidyverse/ggplot2' \
    'thomasp85/gganimate' \
    'thomasp85/scico' && \
  Rscript -e 'remotes::install_git("https://gitlab.com/uribo/jmastats")'
