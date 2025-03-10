FROM rocker/geospatial:latest

WORKDIR /code

RUN install2.r --error \
    bench \
    bsicons \
    bslib \
    duckdbfs \
    fontawesome \
    gt \
    markdown \
    shiny \
    shinybusy \
    tidyverse \
    colourpicker

RUN installGithub.r cboettig/mapgl tidyverse/ellmer cboettig/duckdbfs

COPY . .

CMD ["R", "--quiet", "-e", "shiny::runApp(host='0.0.0.0', port=8080)"]
