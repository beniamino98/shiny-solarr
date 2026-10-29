# ---------------------------------------------------------------------------
# Sidebar module: data selection + full model specification.
# Returns a reactive list with every value build_spec() needs.
# ---------------------------------------------------------------------------

num <- function(id, label, value, min = NA, max = NA, step = NA) {
  numericInput(id, label, value = value, min = min, max = max, step = step)
}

mod_spec_ui <- function(id) {
  ns <- NS(id)
  tagList(
    accordion(
      id = ns("acc"),
      open = c("Data"),
      multiple = TRUE,

      accordion_panel(
        "Data", icon = icon("database"),
        selectInput(ns("place"), "Location", choices = CAMS_PLACES, selected = "Bologna"),
        radioButtons(ns("target"), "Target variable",
                     choices = c(GHI = "GHI", `Clear sky` = "clearsky"),
                     selected = "GHI", inline = TRUE),
        dateRangeInput(ns("sample"), "Full sample", startview = "year"),
        dateRangeInput(ns("train"), "Training window", startview = "year"),
        helpText("Observations outside the training window get zero weight and ",
                 "form the test sample.")
      ),

      accordion_panel(
        "Risk driver", icon = icon("sun"),
        checkboxInput(ns("stochastic_clearsky"), "Stochastic clear sky", FALSE),
        num(ns("clearsky_threshold"), "Clear-sky threshold", 1.01, 1, 1.5, 0.005),
        hr(),
        selectInput(ns("tr_link"), "Link function", LINK_CHOICES, selected = "invgumbel"),
        layout_columns(
          col_widths = c(6, 6),
          num(ns("tr_min_pos"), "Min position", 1, 1, 20, 1),
          num(ns("tr_max_pos"), "Max position", 1, 1, 20, 1)
        ),
        layout_columns(
          col_widths = c(6, 6),
          num(ns("tr_delta"), "delta", 0.05, 0, 0.5, 0.01),
          num(ns("tr_threshold"), "threshold", 0.01, 0, 0.5, 0.005)
        )
      ),

      accordion_panel(
        "Clear-sky model", icon = icon("cloud-sun"),
        selectInput(ns("cs_method"), "Fitting method", CLEARSKY_METHODS, selected = "OLS"),
        selectInput(ns("cs_method_H0"), "Extraterrestrial radiation",
                    CLEARSKY_H0_METHODS, selected = "spencer"),
        layout_columns(
          col_widths = c(6, 6),
          num(ns("cs_order"), "Order", 1, 0, 6, 1),
          num(ns("cs_order_H0"), "Order H0", 1, 0, 6, 1)
        ),
        num(ns("cs_period"), "Period", 365, 1, 366, 1),
        checkboxInput(ns("cs_intercept"), "Include intercept", TRUE),
        checkboxInput(ns("cs_trend"), "Include trend", FALSE)
      ),

      accordion_panel(
        "Seasonal mean", icon = icon("chart-line"),
        layout_columns(
          col_widths = c(6, 6),
          num(ns("sm_order"), "Order", 1, 0, 6, 1),
          num(ns("sm_period"), "Period", 365, 1, 366, 1)
        ),
        checkboxInput(ns("sm_intercept"), "Include intercept", TRUE),
        checkboxInput(ns("sm_trend"), "Include trend", FALSE),
        checkboxInput(ns("sm_monthly"), "Monthly means", FALSE)
      ),

      accordion_panel(
        "ARMA mean model", icon = icon("wave-square"),
        layout_columns(
          col_widths = c(6, 6),
          num(ns("ar_order"), "AR order (p)", 1, 0, 6, 1),
          num(ns("ma_order"), "MA order (q)", 0, 0, 6, 1)
        ),
        checkboxInput(ns("arma_intercept"), "Include intercept", FALSE)
      ),

      accordion_panel(
        "Seasonal variance", icon = icon("chart-column"),
        layout_columns(
          col_widths = c(6, 6),
          num(ns("sv_order"), "Order", 1, 0, 6, 1),
          num(ns("sv_period"), "Period", 365, 1, 366, 1)
        ),
        checkboxInput(ns("sv_trend"), "Include trend", FALSE),
        checkboxInput(ns("sv_correction"), "Variance correction", FALSE),
        checkboxInput(ns("sv_monthly"), "Monthly std. deviations", FALSE)
      ),

      accordion_panel(
        "GARCH variance", icon = icon("chart-area"),
        layout_columns(
          col_widths = c(6, 6),
          num(ns("arch_order"), "ARCH order (r)", 1, 0, 3, 1),
          num(ns("garch_order"), "GARCH order (s)", 1, 0, 3, 1)
        ),
        helpText("Set both to 0 to switch the GARCH component off.")
      ),

      accordion_panel(
        "Gaussian mixture", icon = icon("layer-group"),
        selectInput(ns("mx_method"), "EM implementation", MIXTURE_METHODS,
                    selected = "mclust"),
        checkboxInput(ns("mx_match_exp"), "Match expectation", FALSE),
        checkboxInput(ns("mx_match_var"), "Match variance", FALSE),
        checkboxInput(ns("mx_match_emp"), "Match empirical moments", FALSE),
        num(ns("mx_abstol"), "Convergence tolerance", 1e-20, 0, 1, 1e-20),
        layout_columns(
          col_widths = c(6, 6),
          num(ns("mx_maxit"), "Max iterations", 5000, 10, 1e5, 100),
          num(ns("mx_maxrestarts"), "Max restarts", 500, 1, 1e4, 50)
        )
      )
    ),

    div(
      class = "d-grid gap-2 mt-3",
      actionButton(ns("fit"), "Fit model", icon = icon("play"), class = "btn-primary"),
      actionButton(ns("reset"), "Reset to defaults", class = "btn-outline-secondary btn-sm")
    ),
    hr(),
    fileInput(ns("upload"), "Load a fitted model (.rds)", accept = ".rds",
              buttonLabel = "Browse", placeholder = "solarModel object")
  )
}

mod_spec_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Date pickers follow the coverage of the selected location.
    observeEvent(input$place, {
      d <- solarr::CAMS_data[[input$place]]
      rng <- range(d$date, na.rm = TRUE)
      # Default: hold out the last two years as test sample.
      train_to <- max(rng[1], rng[2] - 730)
      updateDateRangeInput(session, "sample", start = rng[1], end = rng[2],
                           min = rng[1], max = rng[2])
      updateDateRangeInput(session, "train", start = rng[1], end = train_to,
                           min = rng[1], max = rng[2])
    }, ignoreInit = FALSE)

    observeEvent(input$reset, {
      updateRadioButtons(session, "target", selected = "GHI")
      updateCheckboxInput(session, "stochastic_clearsky", value = FALSE)
      updateNumericInput(session, "clearsky_threshold", value = 1.01)
      updateSelectInput(session, "tr_link", selected = "invgumbel")
      updateNumericInput(session, "tr_min_pos", value = 1)
      updateNumericInput(session, "tr_max_pos", value = 1)
      updateNumericInput(session, "tr_delta", value = 0.05)
      updateNumericInput(session, "tr_threshold", value = 0.01)
      updateSelectInput(session, "cs_method", selected = "OLS")
      updateSelectInput(session, "cs_method_H0", selected = "spencer")
      updateNumericInput(session, "cs_order", value = 1)
      updateNumericInput(session, "cs_order_H0", value = 1)
      updateNumericInput(session, "cs_period", value = 365)
      updateCheckboxInput(session, "cs_intercept", value = TRUE)
      updateCheckboxInput(session, "cs_trend", value = FALSE)
      updateNumericInput(session, "sm_order", value = 1)
      updateNumericInput(session, "sm_period", value = 365)
      updateCheckboxInput(session, "sm_intercept", value = TRUE)
      updateCheckboxInput(session, "sm_trend", value = FALSE)
      updateCheckboxInput(session, "sm_monthly", value = FALSE)
      updateNumericInput(session, "ar_order", value = 1)
      updateNumericInput(session, "ma_order", value = 0)
      updateCheckboxInput(session, "arma_intercept", value = FALSE)
      updateNumericInput(session, "sv_order", value = 1)
      updateNumericInput(session, "sv_period", value = 365)
      updateCheckboxInput(session, "sv_trend", value = FALSE)
      updateCheckboxInput(session, "sv_correction", value = FALSE)
      updateCheckboxInput(session, "sv_monthly", value = FALSE)
      updateNumericInput(session, "arch_order", value = 1)
      updateNumericInput(session, "garch_order", value = 1)
      updateSelectInput(session, "mx_method", selected = "mclust")
      updateCheckboxInput(session, "mx_match_exp", value = FALSE)
      updateCheckboxInput(session, "mx_match_var", value = FALSE)
      updateCheckboxInput(session, "mx_match_emp", value = FALSE)
      updateNumericInput(session, "mx_abstol", value = 1e-20)
      updateNumericInput(session, "mx_maxit", value = 5000)
      updateNumericInput(session, "mx_maxrestarts", value = 500)
      showNotification("Specification reset to package defaults.", type = "message")
    })

    config <- reactive({
      list(
        place = input$place, target = input$target,
        sample_from = input$sample[1], sample_to = input$sample[2],
        train_from  = input$train[1],  train_to  = input$train[2],
        stochastic_clearsky = isTRUE(input$stochastic_clearsky),
        clearsky_threshold = input$clearsky_threshold,
        tr_link = input$tr_link, tr_min_pos = input$tr_min_pos,
        tr_max_pos = input$tr_max_pos, tr_delta = input$tr_delta,
        tr_threshold = input$tr_threshold,
        cs_method = input$cs_method, cs_order = input$cs_order,
        cs_order_H0 = input$cs_order_H0, cs_method_H0 = input$cs_method_H0,
        cs_period = input$cs_period, cs_intercept = isTRUE(input$cs_intercept),
        cs_trend = isTRUE(input$cs_trend),
        sm_order = input$sm_order, sm_period = input$sm_period,
        sm_trend = isTRUE(input$sm_trend), sm_intercept = isTRUE(input$sm_intercept),
        sm_monthly = isTRUE(input$sm_monthly),
        ar_order = input$ar_order, ma_order = input$ma_order,
        arma_intercept = isTRUE(input$arma_intercept),
        sv_order = input$sv_order, sv_period = input$sv_period,
        sv_trend = isTRUE(input$sv_trend), sv_correction = isTRUE(input$sv_correction),
        sv_monthly = isTRUE(input$sv_monthly),
        arch_order = input$arch_order, garch_order = input$garch_order,
        mx_method = input$mx_method, mx_match_exp = isTRUE(input$mx_match_exp),
        mx_match_var = isTRUE(input$mx_match_var),
        mx_match_emp = isTRUE(input$mx_match_emp),
        mx_abstol = input$mx_abstol, mx_maxit = input$mx_maxit,
        mx_maxrestarts = input$mx_maxrestarts
      )
    })

    # Make the sidebar mirror a model loaded from disk, so the Data panel and
    # the controls describe the object actually on screen.
    sync_from_model <- function(model) {
      spec <- model$spec
      d <- spec$dates
      if (spec$place %in% CAMS_PLACES) {
        rng <- range(solarr::CAMS_data[[spec$place]]$date, na.rm = TRUE)
        updateSelectInput(session, "place", selected = spec$place)
        updateDateRangeInput(session, "sample", start = d$data$from, end = d$data$to,
                             min = rng[1], max = rng[2])
        updateDateRangeInput(session, "train", start = d$train$from, end = d$train$to,
                             min = rng[1], max = rng[2])
      }
      updateRadioButtons(session, "target", selected = spec$target)
      updateCheckboxInput(session, "stochastic_clearsky", value = spec$stochastic_clearsky)
      updateNumericInput(session, "clearsky_threshold", value = spec$clearsky_threshold)
      updateSelectInput(session, "tr_link", selected = spec$transform$link)
      ctrl <- spec$transform$control
      updateNumericInput(session, "tr_min_pos", value = ctrl$min_pos)
      updateNumericInput(session, "tr_max_pos", value = ctrl$max_pos)
      updateNumericInput(session, "tr_delta", value = ctrl$delta)
      updateNumericInput(session, "tr_threshold", value = ctrl$threshold)
      updateNumericInput(session, "sm_order", value = spec$seasonal.mean$order)
      updateNumericInput(session, "sm_period", value = spec$seasonal.mean$period)
      updateNumericInput(session, "ar_order", value = spec$mean.model$arOrder)
      updateNumericInput(session, "ma_order", value = spec$mean.model$maOrder)
      updateNumericInput(session, "sv_order", value = spec$seasonal.variance$order)
      updateNumericInput(session, "sv_period", value = spec$seasonal.variance$period)
      updateNumericInput(session, "arch_order", value = spec$variance.model$archOrder)
      updateNumericInput(session, "garch_order", value = spec$variance.model$garchOrder)
      updateSelectInput(session, "mx_method", selected = mixture_method(spec))
    }

    list(
      config = config,
      place  = reactive(input$place),
      sample = reactive(input$sample),
      train  = reactive(input$train),
      fit    = reactive(input$fit),
      upload = reactive(input$upload),
      sync   = sync_from_model
    )
  })
}
