# shiny-certificate

[![gh-actions-build-status](https://github.com/royfrancis/shiny-certificate/workflows/build/badge.svg)](https://github.com/royfrancis/shiny-certificate/actions?workflow=build) [![Docker Image Size (latest by date)](https://img.shields.io/docker/image-size/royfrancis/shiny-certificate?label=dockerhub)](https://hub.docker.com/repository/docker/royfrancis/shiny-certificate)

This is an R shiny app to create certificates for NBIS workshops.

![](preview.png)

## Running the app

### Run online

Click [here](https://roymf.shinyapps.io/certificate/) to access an online instance of this app. This link may not always be active.

### Run using docker

```
docker run --rm -p 8787:8787 royfrancis/shiny-certificate:v1.1.5
```

The app should be available through a web browser at `http://0.0.0.0:8787`.

### Run in R

Install the following R packages:

```
install.packages(c(Cairo, ggplot2, ggtext, magick, png, shiny, shinyBS, shinythemes, showtext))
```

This repo is not an R package. Clone this repo and execute the R command `shiny::runApp("app.R")` in the root directory.

2021 • Roy Francis
