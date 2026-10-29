# ---------------------------------------------------------------------------
# Tab "Data": raw series for the selected location, before any estimation.
# ---------------------------------------------------------------------------

mod_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = cols_4(),
      kpi_box("Observations", textOutput(ns("vb_nobs"), inline = TRUE), "primary"),
      kpi_box("Coverage", textOutput(ns("vb_range"), inline = TRUE), "secondary"),
      kpi_box("Mean GHI", textOutput(ns("vb_mean"), inline = TRUE),
              "secondary", unit = "kWh/m2"),
      kpi_box("Missing values", textOutput(ns("vb_na"), inline = TRUE), "secondary")
    ),
    # No layout_columns() wrapper around a single card: bslib would size that
    # grid to one column and the `--bs-columns: 1` it sets cascades into the
    # control grid nested inside the card.
    card(
      card_header("Observed radiation and clear sky"),
      card_body(
        layout_columns(
          col_widths = cols_row(6, 3, 3),
          sliderInput(ns("years"), "Years shown", min = 2000, max = 2030,
                      value = c(2000, 2030), step = 1, sep = "", ticks = FALSE),
          div(class = "sl-inline-check",
              checkboxInput(ns("show_clearsky"), "Show clear sky", TRUE)),
          div(class = "sl-inline-check",
              checkboxInput(ns("show_window"), "Shade the test window", TRUE))
        ),
        plotlyOutput(ns("ts_plot"), height = "380px")
      )
    ),
    layout_columns(
      col_widths = cols_2(),
      card(card_header("Seasonal profile"), card_body(plotOutput(ns("season_plot"), height = "320px"))),
      card(card_header("Monthly distribution"), card_body(plotOutput(ns("month_plot"), height = "320px")))
    ),
    card(card_header("Raw data"), card_body(DTOutput(ns("table"))))
  )
}

mod_data_server <- function(id, place, sample_range, train_range) {
  moduleServer(id, function(input, output, session) {

    raw <- reactive({
      req(place())
      d <- solarr::CAMS_data[[place()]]
      rng <- sample_range()
      if (!is.null(rng) && all(!is.na(rng))) {
        d <- d[d$date >= rng[1] & d$date <= rng[2], ]
      }
      d
    })

    observeEvent(raw(), {
      req(nrow(raw()) > 0)
      yrs <- range(raw()$Year, na.rm = TRUE)
      updateSliderInput(session, "years", min = yrs[1], max = yrs[2],
                        value = c(max(yrs[1], yrs[2] - 4), yrs[2]))
    })

    shown <- reactive({
      d <- raw()
      req(input$years)
      d[d$Year >= input$years[1] & d$Year <= input$years[2], ]
    })

    output$vb_nobs  <- renderText(format(nrow(raw()), big.mark = ","))
    output$vb_range <- renderText({
      req(nrow(raw()) > 0)
      r <- range(raw()$date, na.rm = TRUE)
      sprintf("%s - %s", format(r[1], "%Y"), format(r[2], "%Y"))
    })
    output$vb_mean  <- renderText({
      req(nrow(raw()) > 0)
      sprintf("%.2f", mean(raw()$GHI, na.rm = TRUE))
    })
    output$vb_na    <- renderText(format(sum(is.na(raw()$GHI))))

    output$ts_plot <- renderPlotly({
      d <- shown()
      validate(need(nrow(d) > 0, "No observations in the selected years."))
      p <- ggplot(d, aes(x = date))
      if (isTRUE(input$show_clearsky)) {
        p <- p + geom_line(aes(y = clearsky, colour = "Clear sky"), linewidth = 0.3)
      }
      p <- p + geom_line(aes(y = GHI, colour = "GHI"), linewidth = 0.3) +
        scale_colour_manual(values = c(GHI = PAL[["main"]], `Clear sky` = PAL[["accent"]])) +
        labs(x = NULL, y = "kWh/m2") +
        theme_solar()
      out <- plotly_clean(p)

      # The band goes on after the conversion, as a paper-height shape.
      if (isTRUE(input$show_window)) {
        tr <- train_range()
        if (!is.null(tr) && all(!is.na(tr)) && tr[2] < max(d$date)) {
          out <- plotly_shade_x(out, max(tr[2], min(d$date)), max(d$date),
                                label = "test")
        }
      }
      out
    })

    output$season_plot <- renderPlot({
      d <- raw()
      df <- d |>
        dplyr::group_by(n) |>
        dplyr::summarise(ghi_mean = mean(GHI, na.rm = TRUE),
                         cs_mean  = mean(clearsky, na.rm = TRUE),
                         lo = stats::quantile(GHI, 0.1, na.rm = TRUE),
                         hi = stats::quantile(GHI, 0.9, na.rm = TRUE),
                         .groups = "drop")
      ggplot(df, aes(x = n)) +
        geom_ribbon(aes(ymin = lo, ymax = hi), fill = PAL[["main"]], alpha = 0.15) +
        geom_line(aes(y = ghi_mean, colour = "Mean GHI"), linewidth = 0.6) +
        geom_line(aes(y = cs_mean, colour = "Mean clear sky"), linewidth = 0.6) +
        scale_colour_manual(values = c(`Mean GHI` = PAL[["main"]],
                                       `Mean clear sky` = PAL[["accent"]])) +
        labs(x = "Day of the year", y = "kWh/m2",
             subtitle = "Ribbon: 10th-90th percentile across years") +
        theme_solar()
    })

    output$month_plot <- renderPlot({
      d <- raw()
      d$MonthLab <- factor(MONTH_ABB[d$Month], levels = MONTH_ABB)
      ggplot(d, aes(x = MonthLab, y = GHI)) +
        geom_boxplot(fill = "grey90", colour = PAL[["main"]], outlier.size = 0.4,
                     outlier.alpha = 0.4, linewidth = 0.35) +
        labs(x = NULL, y = "GHI (kWh/m2)") +
        theme_solar()
    })

    output$table <- renderDT({
      d <- raw()
      sl_table(d, page_length = 12, searchable = TRUE, digits = 4, plain = TRUE,
               round_cols = intersect(c("GHI", "clearsky", "H0"), names(d)))
    })

    list(raw = raw)
  })
}
