# ---------------------------------------------------------------------------
# Tab "Solar geometry": the deterministic quantities behind the model.
#
# Nothing here depends on a fitted model. The site comes from the location
# chosen in the sidebar, or from coordinates typed in by hand.
# ---------------------------------------------------------------------------

mod_solar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_body(
        class = "py-3",
        layout_columns(
          col_widths = cols_row(3, 2, 4, 3),
          uiOutput(ns("site_label")),
          selectInput(ns("method"), "Formulas",
                      choices = c(Spencer = "spencer", Cooper = "cooper"),
                      selected = "spencer"),
          selectInput(ns("convention"), "Sunrise / sunset defined as",
                      choices = solar_conventions(), selected = "geometric"),
          numericInput(ns("year"), "Calendar year", value = 2021,
                       min = 1990, max = 2100, step = 1)
        ),
        uiOutput(ns("convention_note")),
        div(
          class = "mt-2",
          checkboxInput(ns("custom"), "Use custom coordinates", FALSE),
          conditionalPanel(
            condition = "input.custom", ns = ns,
            layout_columns(
              col_widths = cols_row(4, 4, 4),
              numericInput(ns("lat"), "Latitude", value = 44.4949, step = 0.5),
              numericInput(ns("lon"), "Longitude", value = 11.3426, step = 0.5),
              numericInput(ns("alt"), "Altitude (m)", value = 71, step = 10)
            )
          )
        )
      )
    ),

    navset_card_underline(
      id = ns("tabs"),

      nav_panel(
        "Seasonal cycle",
        div(
          class = "pt-3",
          layout_columns(
            col_widths = cols_split(4, 8),
            selectInput(ns("variable"), "Quantity", choices = SOLAR_VAR_CHOICES,
                        selected = "declination"),
            div(class = "pt-2", uiOutput(ns("variable_note")))
          ),
          plotOutput(ns("seasonal_plot"), height = "380px")
        )
      ),

      nav_panel(
        "All curves",
        p(class = "sl-note pt-3",
          "Every deterministic quantity over the year, on its own scale. ",
          "They all follow from the declination, which is why the shapes rhyme."),
        plotOutput(ns("all_plot"), height = "560px")
      ),

      nav_panel(
        "Intraday profile",
        div(
          class = "pt-3",
          layout_columns(
            col_widths = cols_row(3, 2, 2, 3, 2),
            dateInput(ns("day"), "Date", value = "2021-06-21"),
            numericInput(ns("beta"), "Tilt (deg)", value = 0, min = 0, max = 90, step = 5),
            numericInput(ns("gamma"), "Azimuth (deg)", value = 0, min = -180, max = 180, step = 15),
            selectInput(ns("clime"), "Hottel correction", choices = CLIME_CHOICES),
            numericInput(ns("lon_st"), "Std. meridian", value = 15, step = 15)
          ),
          uiOutput(ns("intraday_boxes")),
          layout_columns(
            col_widths = cols_2(),
            card(card_header("Sun position"),
                 card_body(plotOutput(ns("position_plot"), height = "300px"))),
            card(card_header("Clear-sky irradiance"),
                 card_body(plotOutput(ns("irradiance_plot"), height = "300px")))
          )
        )
      ),

      nav_panel(
        "Sun path",
        p(class = "sl-note pt-3",
          "Left: the track of the sun across the sky on the solstices, the ",
          "equinoxes and the date selected in the intraday panel. Right: the ",
          "analemma, the equation of time plotted against the declination."),
        layout_columns(
          col_widths = cols_split(7, 5),
          card(card_header("Sun path"),
               card_body(plotOutput(ns("sunpath_plot"), height = "360px"))),
          card(card_header("Analemma"),
               card_body(plotOutput(ns("analemma_plot"), height = "360px")))
        )
      ),

      nav_panel(
        "Values",
        div(
          class = "pt-3",
          layout_columns(
            col_widths = cols_split(5, 7),
            # fill = FALSE: these two tables have different heights and should
            # not be stretched to a common one.
            card(card_header("Site"), card_body(DTOutput(ns("facts"))), fill = FALSE),
            card(card_header("Definitions"), card_body(DTOutput(ns("glossary"))),
                 fill = FALSE)
          ),
          card(
            card_header("Day-by-day values"),
            card_body(
              div(class = "sl-btn-row mb-2",
                  downloadButton(ns("dl_seasonal"), "Download CSV",
                                 class = "btn-outline-secondary btn-sm")),
              DTOutput(ns("seasonal_table"))
            )
          )
        )
      )
    )
  )
}

mod_solar_server <- function(id, place) {
  moduleServer(id, function(input, output, session) {

    # Typing coordinates by hand overrides the sidebar location.
    observeEvent(place(), {
      co <- place_coords(place())
      updateNumericInput(session, "lat", value = co$lat)
      updateNumericInput(session, "lon", value = co$lon)
      updateNumericInput(session, "alt", value = co$alt)
    }, ignoreInit = FALSE)

    site <- reactive({
      if (isTRUE(input$custom)) {
        req(input$lat, input$lon, input$alt)
        list(name = "custom coordinates", lat = input$lat, lon = input$lon, alt = input$alt)
      } else {
        req(place())
        co <- place_coords(place())
        list(name = place(), lat = co$lat, lon = co$lon, alt = co$alt)
      }
    })

    output$convention_note <- renderUI({
      cv <- input$convention %||% "geometric"
      div(
        class = "mt-1",
        span(class = "sl-badge",
             sprintf("h0 = %+.4f deg",
                     solarr::seasonalSolarFunctions$new()$horizons[[cv]])),
        span(class = "sl-note ms-1", CONVENTION_NOTES[[cv]]),
        p(class = "sl-note mb-0 mt-1",
          "Sunrise, sunset, day length and the sunset hour angle follow this ",
          "choice. The extraterrestrial radiation H_on does not: above the ",
          "atmosphere there is no refraction, so it stays geometric and keeps ",
          "matching the H0 the clear-sky model is fitted on.")
      )
    })

    output$site_label <- renderUI({
      s <- site()
      div(
        div(class = "sl-subhead", "Site"),
        div(class = "fw-semibold", s$name),
        div(class = "text-muted small",
            sprintf("lat %.4f / lon %.4f / alt %s m", s$lat, s$lon, s$alt))
      )
    })

    seasonal <- reactive({
      s <- site()
      req(input$year, input$method, input$convention)
      solar_seasonal(s$lat, s$alt, year = input$year, method = input$method,
                     convention = input$convention)
    })

    intraday <- reactive({
      s <- site()
      req(input$day, input$method)
      solar_intraday(s$lat, s$lon, s$alt, input$day, method = input$method,
                     beta = input$beta %||% 0, gamma = input$gamma %||% 0,
                     lon_st = input$lon_st %||% 15,
                     clime = input$clime %||% "No Correction",
                     convention = input$convention %||% "geometric")
    })

    # ---------------------------------------------------------------------- #
    # Seasonal cycle
    # ---------------------------------------------------------------------- #
    output$variable_note <- renderUI({
      v <- SOLAR_VARS[[input$variable]]
      div(
        span(class = "sl-badge", v$symbol),
        span(class = "sl-badge", v$unit),
        p(class = "sl-note mt-2 mb-0", v$note)
      )
    })

    output$seasonal_plot <- renderPlot({
      v <- SOLAR_VARS[[input$variable]]
      gg_solar_curve(seasonal(), input$variable,
                     title = v$label, ylab = sprintf("%s [%s]", v$symbol, v$unit))
    })

    output$all_plot <- renderPlot(gg_solar_all(seasonal()))

    # ---------------------------------------------------------------------- #
    # Intraday
    # ---------------------------------------------------------------------- #
    day_facts <- reactive({
      d <- intraday()
      step_h <- as.numeric(difftime(d$clocktime[2], d$clocktime[1], units = "hours"))

      # Sunrise and sunset come from the sunset hour angle the package returns,
      # so they are exact rather than rounded to the time step.
      omega_max <- d$omega_max[1]
      polar <- omega_max >= 180 || omega_max <= 0
      at <- function(w) {
        if (polar) return("-")
        format(solar_event_time(d, w), "%H:%M")
      }

      list(
        sunrise = at(-omega_max),
        sunset  = at(omega_max),
        length  = d$day_length[1],
        peak    = max(d$clearsky),
        total   = sum(d$clearsky) * step_h / 1000,
        tilted  = sum(d$beam_tilted) * step_h / 1000
      )
    })

    output$intraday_boxes <- renderUI({
      f <- day_facts()
      layout_columns(
        col_widths = cols_4(),
        kpi_box("Sunrise / sunset", sprintf("%s-%s", f$sunrise, f$sunset), "primary"),
        kpi_box("Day length", sprintf("%.2f", f$length), unit = "h"),
        kpi_box("Peak", sprintf("%.0f", f$peak), unit = "W/m2"),
        kpi_box("Daily total", sprintf("%.2f", f$total), unit = "kWh/m2")
      )
    })

    output$position_plot <- renderPlot(gg_sun_position(intraday()))
    output$irradiance_plot <- renderPlot({
      gg_sun_irradiance(intraday(), tilted = (input$beta %||% 0) != 0)
    })

    # ---------------------------------------------------------------------- #
    # Sun path and analemma
    # ---------------------------------------------------------------------- #
    output$sunpath_plot <- renderPlot({
      s <- site()
      yr <- input$year %||% 2021
      key <- c(`Winter solstice` = sprintf("%d-12-21", yr),
               `Equinox`         = sprintf("%d-03-20", yr),
               `Summer solstice` = sprintf("%d-06-21", yr))
      # Only add the selected date when it is not already one of the three:
      # an exact overlap hides a curve behind another and confuses the legend.
      if (!is.null(input$day) && !as.character(input$day) %in% key) {
        key <- c(key, stats::setNames(as.character(input$day), format(input$day, "%d %b")))
      }
      paths <- do.call(rbind, lapply(seq_along(key), function(i) {
        d <- solar_intraday(s$lat, s$lon, s$alt, key[i], method = input$method,
                            lon_st = input$lon_st %||% 15,
                            convention = input$convention %||% "geometric")
        d$label <- names(key)[i]
        d[d$altitude > 0 & is.finite(d$azimuth), c("azimuth", "altitude", "label")]
      }))
      paths$label <- factor(paths$label, levels = names(key))
      gg_sun_path(paths)
    })

    output$analemma_plot <- renderPlot(gg_analemma(seasonal()))

    # ---------------------------------------------------------------------- #
    # Values
    # ---------------------------------------------------------------------- #
    output$facts <- renderDT({
      s <- site()
      sl_kv_table(solar_site_facts(seasonal(), s$lat, s$lon, s$alt))
    })

    output$glossary <- renderDT({
      g <- data.frame(
        Quantity = vapply(SOLAR_VARS, function(x) x$label, character(1)),
        Symbol   = vapply(SOLAR_VARS, function(x) x$symbol, character(1)),
        Unit     = vapply(SOLAR_VARS, function(x) x$unit, character(1)),
        stringsAsFactors = FALSE
      )
      rownames(g) <- NULL
      sl_table(g, page_length = NULL, digits = NULL, scroll_x = FALSE,
               ordering = FALSE, wrap = TRUE)
    })

    seasonal_display <- reactive({
      d <- seasonal()
      names(d) <- c("Date", "Day", "Declination", "Equation of time", "G_on",
                    "H_on", "Sunset hour angle", "Day length", "Noon altitude")
      d
    })

    output$seasonal_table <- renderDT({
      sl_table(seasonal_display(), page_length = 12, searchable = TRUE, digits = 5)
    })

    output$dl_seasonal <- downloadHandler(
      filename = function() {
        sprintf("solar-geometry-%s-%s.csv",
                gsub("[^A-Za-z0-9]+", "-", site()$name), input$year)
      },
      content = function(file) utils::write.csv(seasonal(), file, row.names = FALSE)
    )
  })
}
