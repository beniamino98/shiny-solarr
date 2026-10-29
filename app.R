# ---------------------------------------------------------------------------
# solarModel
#
# Estimate a solarr::solarModel on CAMS data, inspect the residuals produced at
# every stage of the fitting pipeline, and run the diagnostic tests shipped
# with the package.
#
# Run with:  shiny::runApp("shiny-solarr")
#
# Packages, shared constants and every helper live under R/, which Shiny
# sources before this file. See R/aaa_setup.R.
# ---------------------------------------------------------------------------

ui <- page_navbar(
  title = APP_TITLE,
  id = "nav",
  theme = bs_theme(
    version = 5,
    preset = "shiny",
    primary = "#1f4e79",
    secondary = "#6b7885",
    success = "#2e7d32",
    danger = "#c62828",
    "navbar-bg" = "#1f4e79",
    "body-bg" = "#f4f6f9",
    "card-cap-bg" = "#ffffff",
    base_font = font_google("Source Sans 3", local = FALSE),
    code_font = font_google("JetBrains Mono", local = FALSE)
  ),
  fillable = FALSE,
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),

  sidebar = sidebar(
    width = 340,
    title = "Specification",
    mod_spec_ui("spec")
  ),

  nav_panel("Data", icon = icon("table"), mod_data_ui("data")),
  nav_panel("Solar geometry", icon = icon("sun"), mod_solar_ui("solar")),
  nav_panel("Estimation", icon = icon("sliders"), mod_estimate_ui("estimate")),
  nav_panel("Residuals", icon = icon("wave-square"), mod_residuals_ui("residuals")),
  nav_panel("Diagnostics", icon = icon("stethoscope"), mod_diagnostics_ui("diagnostics")),
  nav_panel("Export", icon = icon("download"), mod_export_ui("export")),
  nav_spacer(),
  nav_item(uiOutput("model_badge"))
)

server <- function(input, output, session) {

  spec_inputs <- mod_spec_server("spec")

  # --- the fitted model -----------------------------------------------------
  model <- reactiveVal(NULL)
  fit_log <- reactiveVal(NULL)

  observeEvent(spec_inputs$fit(), {
    cfg <- spec_inputs$config()
    if (is.na(cfg$sample_from) || is.na(cfg$sample_to)) {
      showNotification("Pick a valid sample window first.", type = "error")
      return()
    }
    if (cfg$train_from < cfg$sample_from || cfg$train_to > cfg$sample_to) {
      showNotification("The training window must sit inside the full sample.",
                       type = "error", duration = 8)
      return()
    }

    withProgress(message = "Fitting the solar model", value = 0.1, {
      setProgress(0.2, detail = "Building the specification")
      spec <- try(build_spec(cfg), silent = TRUE)
      if (inherits(spec, "try-error")) {
        showNotification(paste("Specification failed:",
                               conditionMessage(attr(spec, "condition"))),
                         type = "error", duration = NULL)
        return()
      }
      setProgress(0.5, detail = "Running the estimation pipeline")
      res <- fit_solar_model(spec)
      setProgress(1)
      if (!res$ok) {
        model(NULL)
        fit_log(res$log)
        showNotification(paste("Estimation failed:", res$error),
                         type = "error", duration = NULL)
      } else {
        model(res$model)
        fit_log(res$log)
        showNotification(sprintf("Model fitted: %s, log-likelihood %s",
                                 res$model$spec$model_name,
                                 format(res$model$loglik, digits = 7)),
                         type = "message", duration = 6)
        nav_select("nav", "Estimation")
      }
    })
  }, ignoreInit = TRUE)

  # --- loading a model estimated elsewhere ----------------------------------
  observeEvent(spec_inputs$upload(), {
    file <- spec_inputs$upload()
    req(file)
    obj <- try(readRDS(file$datapath), silent = TRUE)
    if (inherits(obj, "try-error")) {
      showNotification(paste("Could not read the file:",
                             conditionMessage(attr(obj, "condition"))),
                       type = "error", duration = NULL)
      return()
    }
    if (!is_solar_model(obj)) {
      showNotification(sprintf("The file holds a '%s', not a solarModel.",
                               paste(class(obj), collapse = "/")),
                       type = "error", duration = 10)
      return()
    }
    if (length(obj$moments$conditional) == 1) {
      showNotification("The uploaded model has no conditional moments: it looks unfitted.",
                       type = "warning", duration = 10)
    }
    model(obj)
    fit_log(NULL)
    spec_inputs$sync(obj)
    showNotification(sprintf("Loaded %s (%s).", obj$spec$place, obj$spec$model_name),
                     type = "message", duration = 6)
    nav_select("nav", "Estimation")
  }, ignoreInit = TRUE)

  # `model$data` rebuilds a join on every access: compute it once per fit.
  fitted_data <- reactive({
    req(model())
    model_frame(model())
  })

  output$model_badge <- renderUI({
    m <- model()
    if (is.null(m)) {
      span(class = "navbar-text text-white-50 pe-3", "No model fitted")
    } else {
      span(class = "navbar-text text-white pe-3",
           sprintf("%s - %s", m$spec$place, m$spec$model_name))
    }
  })

  # --- panels ---------------------------------------------------------------
  mod_data_server("data", spec_inputs$place, spec_inputs$sample, spec_inputs$train)
  mod_solar_server("solar", spec_inputs$place)
  est  <- mod_estimate_server("estimate", model, fitted_data)
  res  <- mod_residuals_server("residuals", model, fitted_data)
  diag <- mod_diagnostics_server("diagnostics", model, fitted_data)
  mod_export_server("export", model, fitted_data, est, diag, fit_log)

  # Panels other than Data need a model: say so instead of showing empty cards.
  observeEvent(input$nav, {
    if (input$nav %in% c("Estimation", "Residuals", "Diagnostics", "Export") &&
        is.null(model())) {
      showNotification("Fit a model, or load one from an .rds file, to populate this panel.",
                       type = "warning", duration = 6)
    }
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
