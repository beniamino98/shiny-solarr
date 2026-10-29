# ---------------------------------------------------------------------------
# Session setup.
#
# Shiny sources every file under R/ in alphabetical order, into an environment
# shared by all of them, before app.R is evaluated. Variables created inside
# app.R are NOT visible from here, so the packages and the shared constants
# both live in this file: the leading "aaa_" keeps it first in the order.
# ---------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(ggplot2)
  library(dplyr)
  library(DT)
  library(plotly)
  library(shinyWidgets)
  library(solarr)
})

# `solarModel_test_autocorr(method = "bg")` calls lmtest::bgtest().
HAS_LMTEST <- requireNamespace("lmtest", quietly = TRUE)

# Locations shipped with solarr (CAMS_data is a named list of tibbles).
CAMS_PLACES <- sort(names(solarr::CAMS_data))

options(shiny.maxRequestSize = 200 * 1024^2)  # allow large .rds model uploads
