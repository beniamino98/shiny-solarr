# ---------------------------------------------------------------------------
# Tab "Estimation": what the fit produced.
# ---------------------------------------------------------------------------

mod_estimate_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = cols_4(),
      kpi_box("Log-likelihood", textOutput(ns("vb_ll"), inline = TRUE), "primary"),
      kpi_box("AIC", textOutput(ns("vb_aic"), inline = TRUE)),
      kpi_box("BIC", textOutput(ns("vb_bic"), inline = TRUE)),
      kpi_box("Parameters", textOutput(ns("vb_k"), inline = TRUE))
    ),

    layout_columns(
      col_widths = cols_split(7, 5),
      card(card_header("Model"), card_body(verbatimTextOutput(ns("print")))),
      card(card_header("Summary"), card_body(DTOutput(ns("summary"))))
    ),

    card(
      card_header("Coefficients"),
      card_body(
        # The table's own search box is switched off so the filter and the
        # checkbox can share one row; the filtering happens server side below.
        div(
          class = "sl-table-toolbar",
          div(class = "sl-toolbar-check",
              checkboxInput(ns("hide_mixture"),
                            "Hide the monthly mixture parameters", TRUE)),
          div(class = "sl-toolbar-search",
              textInput(ns("coef_search"), NULL, placeholder = "Filter rows"))
        ),
        DTOutput(ns("coefs"))
      )
    ),

    layout_columns(
      col_widths = cols_2(),

      do.call(navset_card_underline, c(
        list(title = "Seasonal components"),
        lapply(names(SEASONAL_COMPONENTS), function(v) {
          nav_panel(SEASONAL_COMPONENTS[[v]]$tab,
                    div(class = "pt-2",
                        plotOutput(ns(paste0("seasonal_", v)), height = "290px"),
                        p(class = "sl-note mt-2 mb-0", SEASONAL_COMPONENTS[[v]]$note)))
        })
      )),

      do.call(navset_card_underline, c(
        list(title = "Monthly mixture parameters"),
        lapply(names(MIXTURE_GROUPS), function(g) {
          nav_panel(MIXTURE_GROUPS[[g]]$label,
                    div(class = "pt-2",
                        plotOutput(ns(paste0("mixture_", g)), height = "290px"),
                        p(class = "sl-note mt-2 mb-0", MIXTURE_GROUPS[[g]]$note)))
        })
      ))
    ),

    navset_card_underline(
      title = "Monthly Gaussian mixture",
      nav_panel("Parameters", div(class = "pt-2", DTOutput(ns("mix_params")))),
      nav_panel("Moments", div(class = "pt-2", DTOutput(ns("mix_moments")))),
      nav_panel("Fitted densities",
                div(class = "pt-2", plotOutput(ns("mix_density"), height = "520px")))
    )
  )
}

mod_estimate_server <- function(id, model, fitted_data) {
  moduleServer(id, function(input, output, session) {

    ic <- reactive({
      req(model())
      safely(solarr::solarModel_AIC_BIC(model()))
    })

    ic_field <- function(f) renderText({
      x <- ic()
      if (is_app_error(x)) "-" else format(x[[f]], digits = 8)
    })

    output$vb_ll  <- renderText({ req(model()); format(model()$loglik, digits = 7) })
    output$vb_aic <- ic_field("AIC")
    output$vb_bic <- ic_field("BIC")
    output$vb_k   <- renderText({
      x <- ic()
      if (is_app_error(x)) "-" else as.character(x$k)
    })

    output$print <- renderPrint({ req(model()); model()$print() })

    output$summary <- renderDT({
      req(model())
      sl_kv_table(model_summary(model()))
    })

    coefs <- reactive({
      req(model())
      tab <- tidy_coefficients(model())
      if (isTRUE(input$hide_mixture)) tab <- tab[!grepl("^NM_", tab$component), ]
      tab
    })

    # Same behaviour as the DataTables search box, in our own row.
    coefs_shown <- reactive({
      tab <- coefs()
      q <- trimws(input$coef_search %||% "")
      if (!nzchar(q)) return(tab)
      hit <- Reduce(`|`, lapply(tab, function(col)
        grepl(q, format(col), ignore.case = TRUE, fixed = FALSE)))
      tab[!is.na(hit) & hit, , drop = FALSE]
    })

    output$coefs <- renderDT({
      sl_table(coefs_shown(), page_length = 15, searchable = FALSE, digits = 5,
               signif_cols = c("estimate", "std.error", "statistic"),
               colnames = c("Component", "Parameter", "Estimate", "Std. error",
                            "z", "p-value")) |>
        DT::formatRound("p.value", digits = 5)
    })

    # ---------------------------------------------------------------------- #
    # Seasonal components, one plot per tab
    # ---------------------------------------------------------------------- #
    lapply(names(SEASONAL_COMPONENTS), function(v) {
      output[[paste0("seasonal_", v)]] <- renderPlot({
        req(fitted_data())
        gg_seasonal_component(fitted_data(), v, SEASONAL_COMPONENTS[[v]])
      })
    })

    # ---------------------------------------------------------------------- #
    # Mixture parameters, one plot per tab
    # ---------------------------------------------------------------------- #
    lapply(names(MIXTURE_GROUPS), function(g) {
      output[[paste0("mixture_", g)]] <- renderPlot({
        req(model())
        gg_mixture_group(mixture_params(model()), MIXTURE_GROUPS[[g]])
      })
    })

    output$mix_params <- renderDT({
      req(model())
      sl_table(mixture_params(model()), page_length = NULL, digits = 4,
               ordering = FALSE, round_cols = c("mu1", "mu2", "sd1", "sd2", "p1", "p2"))
    })

    output$mix_moments <- renderDT({
      req(model())
      m <- as.data.frame(model()$spec$mixture.model$moments)
      keep <- intersect(c("Month", "mean", "variance", "skewness", "kurtosis",
                          "nobs", "loglik"), names(m))
      sl_table(m[, keep], page_length = NULL, digits = 4, ordering = FALSE,
               round_cols = setdiff(keep, c("Month", "nobs")))
    })

    output$mix_density <- renderPlot({
      req(model(), fitted_data())
      d <- fitted_data()
      p <- mixture_params(model())
      grid <- seq(min(d$u_tilde, na.rm = TRUE), max(d$u_tilde, na.rm = TRUE),
                  length.out = 400)
      dens <- do.call(rbind, lapply(seq_len(nrow(p)), function(i) {
        row <- p[i, ]
        data.frame(
          Month = row$Month, x = grid,
          mixture = solarr::dmixnorm(grid, c(row$mu1, row$mu2),
                                     c(row$sd1, row$sd2), c(row$p1, row$p2)),
          cloudy  = row$p1 * stats::dnorm(grid, row$mu1, row$sd1),
          sunny   = row$p2 * stats::dnorm(grid, row$mu2, row$sd2)
        )
      }))
      dens$MonthLab <- factor(MONTH_ABB[dens$Month], levels = MONTH_ABB)
      d$MonthLab <- factor(MONTH_ABB[d$Month], levels = MONTH_ABB)

      ggplot() +
        geom_histogram(data = d, aes(x = u_tilde, y = after_stat(density)),
                       bins = 30, fill = "#e8ecf1", colour = "white", linewidth = 0.15) +
        geom_line(data = dens, aes(x = x, y = cloudy, colour = "Cloudy component"),
                  linewidth = 0.45) +
        geom_line(data = dens, aes(x = x, y = sunny, colour = "Sunny component"),
                  linewidth = 0.45) +
        geom_line(data = dens, aes(x = x, y = mixture, colour = "Mixture"),
                  linewidth = 0.75) +
        facet_wrap(~MonthLab, ncol = 4) +
        scale_colour_manual(values = c(Mixture = PAL[["main"]],
                                       `Cloudy component` = PAL[["dw"]],
                                       `Sunny component` = PAL[["up"]])) +
        labs(x = "u_tilde", y = "Density") +
        theme_solar()
    })

    list(coefficients = coefs)
  })
}
