# ---------------------------------------------------------------------------
# Tab "Residuals": one residual series at a time, along the estimation chain.
# ---------------------------------------------------------------------------

mod_residuals_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_body(
        class = "py-3",
        layout_columns(
          col_widths = cols_row(4, 2, 2, 4),
          selectInput(ns("series"), "Residual series", choices = RESIDUAL_CHOICES,
                      selected = "u_tilde"),
          selectInput(ns("type"), "Sample", choices = SAMPLE_TYPES, selected = "full"),
          pickerInput(
            ns("months"), "Months",
            choices  = stats::setNames(1:12, MONTH_LABELS),
            selected = 1:12,
            multiple = TRUE,
            width    = "100%",
            options  = pickerOptions(
              actionsBox        = TRUE,
              selectAllText     = "All",
              deselectAllText   = "None",
              selectedTextFormat = "count > 3",
              countSelectedText = "{0} of {1} months",
              noneSelectedText  = "No month",
              size              = 12,
              style             = "btn-outline-secondary btn-sm"
            )
          ),
          sliderInput(ns("lag_max"), "Maximum lag", min = 10, max = 400,
                      value = 60, step = 10, ticks = FALSE)
        ),
        uiOutput(ns("series_note"))
      )
    ),
    layout_columns(
      col_widths = cols_4(),
      kpi_box("Observations", textOutput(ns("vb_n"), inline = TRUE), "primary"),
      kpi_box("Mean", textOutput(ns("vb_mean"), inline = TRUE), "secondary"),
      kpi_box("Std. deviation", textOutput(ns("vb_sd"), inline = TRUE), "secondary"),
      kpi_box("Skewness / Kurtosis", textOutput(ns("vb_shape"), inline = TRUE), "secondary")
    ),
    card(card_header("Residuals over time"),
         card_body(plotlyOutput(ns("ts_plot"), height = "300px"))),
    layout_columns(
      col_widths = cols_2(),
      card(card_header("Autocorrelation"), card_body(plotOutput(ns("acf_plot"), height = "300px"))),
      card(card_header("Autocorrelation of the squares"),
           card_body(plotOutput(ns("acf2_plot"), height = "300px")))
    ),
    layout_columns(
      col_widths = cols_2(),
      card(card_header("Partial autocorrelation"),
           card_body(plotOutput(ns("pacf_plot"), height = "300px"))),
      card(card_header("Monthly dispersion"),
           card_body(plotOutput(ns("box_plot"), height = "300px")))
    ),
    layout_columns(
      col_widths = cols_2(),
      card(
        card_header("Distribution"),
        card_body(
          radioButtons(ns("reference"), NULL,
                       choices = c(`Empirical only` = "none", Normal = "normal",
                                   `Gaussian mixture` = "mixture"),
                       selected = "normal", inline = TRUE),
          plotOutput(ns("hist_plot"), height = "300px")
        )
      ),
      card(
        card_header("QQ-plot"),
        card_body(
          radioButtons(ns("qq_reference"), NULL,
                       choices = c(Normal = "normal", `Gaussian mixture` = "mixture"),
                       selected = "normal", inline = TRUE),
          plotOutput(ns("qq_plot"), height = "300px")
        )
      )
    )
  )
}

mod_residuals_server <- function(id, model, fitted_data) {
  moduleServer(id, function(input, output, session) {

    meta <- reactive({ req(input$series); RESIDUAL_SERIES[[input$series]] })

    # `u_tilde` and `z_tilde` are the only series with a mixture reference.
    observeEvent(input$series, {
      has_mix <- meta()$reference == "mixture"
      if (!has_mix) {
        if (isTRUE(input$reference == "mixture")) updateRadioButtons(session, "reference", selected = "normal")
        if (isTRUE(input$qq_reference == "mixture")) updateRadioButtons(session, "qq_reference", selected = "normal")
      }
      updateRadioButtons(session, "reference",
                         choices = if (has_mix)
                           c(`Empirical only` = "none", Normal = "normal", `Gaussian mixture` = "mixture")
                         else c(`Empirical only` = "none", Normal = "normal"),
                         selected = if (has_mix) "mixture" else "normal", inline = TRUE)
      updateRadioButtons(session, "qq_reference",
                         choices = if (has_mix)
                           c(Normal = "normal", `Gaussian mixture` = "mixture")
                         else c(Normal = "normal"),
                         selected = "normal", inline = TRUE)
    })

    subset <- reactive({
      req(fitted_data(), input$series)
      d <- filter_sample(fitted_data(), input$type)
      months <- as.integer(input$months %||% 1:12)
      if (length(months) > 0) d <- d[d$Month %in% months, ]
      d[is.finite(d[[input$series]]), ]
    })

    x <- reactive({ subset()[[input$series]] })

    month_weights <- reactive({
      tb <- table(subset()$Month)
      stats::setNames(as.numeric(tb) / sum(tb), names(tb))
    })

    output$series_note <- renderUI({
      m <- meta()
      div(
        class = "mt-1",
        tags$p(class = "mb-1",
               tags$strong(m$stage), " ", tags$code(m$formula)),
        tags$p(class = "sl-note mb-2", m$note),
        tags$span(class = "sl-badge", paste("Series:", m$expected)),
        tags$span(class = "sl-badge", paste("Squares:", m$expected_sq))
      )
    })

    output$vb_n    <- renderText(format(length(x()), big.mark = ","))
    output$vb_mean <- renderText(sprintf("%.4f", mean(x())))
    output$vb_sd   <- renderText(sprintf("%.4f", stats::sd(x())))
    output$vb_shape <- renderText({
      v <- x(); z <- (v - mean(v)) / stats::sd(v)
      sprintf("%.3f / %.3f", mean(z^3), mean(z^4))
    })

    output$ts_plot <- renderPlotly({
      plotly_clean(gg_series(subset(), input$series, ylab = input$series))
    })

    output$acf_plot  <- renderPlot(gg_acf(x(), input$lag_max))
    output$acf2_plot <- renderPlot(gg_acf(x(), input$lag_max, squared = TRUE))
    output$pacf_plot <- renderPlot(gg_acf(x(), input$lag_max, partial = TRUE))
    output$box_plot  <- renderPlot(gg_monthly_box(subset(), input$series))

    output$hist_plot <- renderPlot({
      gg_hist_density(
        x(), reference = input$reference %||% "normal", model = model(),
        months = as.integer(input$months %||% 1:12), weights = month_weights(),
        series = input$series, xlab = input$series
      )
    })

    output$qq_plot <- renderPlot({
      gg_qq(x(), reference = input$qq_reference %||% "normal", model = model(),
            data = subset(), series = input$series)
    })

    list(subset = subset, series = reactive(input$series))
  })
}
