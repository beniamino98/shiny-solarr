# ---------------------------------------------------------------------------
# Tab "Diagnostics": the formal tests exported by solarr.
# ---------------------------------------------------------------------------

mod_diagnostics_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_body(
        class = "py-3",
        layout_columns(
          col_widths = cols_row(3, 3, 3, 3),
          selectInput(ns("type"), "Sample", choices = SAMPLE_TYPES, selected = "full"),
          textInput(ns("lags"), "Lags", value = "1, 7, 30"),
          div(
            class = "sl-field",
            tags$label(class = "form-label", "Options"),
            popover(
              actionButton(ns("adv"), "Advanced settings", icon = icon("sliders"),
                           class = "btn-outline-secondary w-100"),
              title = "Advanced settings",
              numericInput(ns("ci"), "Significance", value = 0.05, min = 0.001,
                           max = 0.2, step = 0.005),
              selectInput(ns("method"), "Autocorrelation test",
                          choices = AUTOCORR_METHODS, selected = "bg"),
              numericInput(ns("min_q"), "KS min quantile", value = 0.025,
                           min = 0, max = 0.5, step = 0.005),
              numericInput(ns("max_q"), "KS max quantile", value = 0.985,
                           min = 0.5, max = 1, step = 0.005),
              options = list(customClass = "sl-popover")
            )
          ),
          div(
            class = "sl-field",
            tags$label(class = "form-label", HTML("&nbsp;")),
            actionButton(ns("run"), "Run diagnostics", icon = icon("stethoscope"),
                         class = "btn-primary w-100")
          )
        )
      )
    ),
    uiOutput(ns("scorecard")),
    navset_card_underline(
      id = ns("tabs"),
      nav_panel(
        "Autocorrelation",
        p(class = "sl-note pt-3",
          "One row per residual series and per lag. ", tags$em("result"),
          " compares the outcome with what a well specified model should produce: ",
          "serial correlation is expected to survive in Yt_tilde and in the squares ",
          "of the intermediate residuals, and to be absent everywhere else."),
        DTOutput(ns("autocorr"))
      ),
      nav_panel(
        "Distribution (KS)",
        p(class = "sl-note pt-3",
          "Kolmogorov-Smirnov test of u_tilde, month by month, against the fitted ",
          "monthly Gaussian mixture and against a normal with the same moments."),
        layout_columns(
          col_widths = cols_2(),
          card(card_header("Against the Gaussian mixture"),
               card_body(DTOutput(ns("ks_gm")))),
          card(card_header("Against the normal"),
               card_body(DTOutput(ns("ks_norm"))))
        ),
        card(card_header("Monthly KS statistics"),
             card_body(plotOutput(ns("ks_plot"), height = "320px")))
      ),
      nav_panel(
        "PIT",
        p(class = "sl-note pt-3",
          "Probability integral transform of the observed radiation through the ",
          "predictive distribution implied by the model. Uniform grades mean the ",
          "whole predictive density, not just its first two moments, is right."),
        layout_columns(
          col_widths = cols_2(),
          card(card_header("Histogram"), card_body(plotOutput(ns("pit_hist"), height = "300px"))),
          card(card_header("Empirical CDF"), card_body(plotOutput(ns("pit_ecdf"), height = "300px")))
        ),
        layout_columns(
          col_widths = cols_2(),
          card(card_header("KS test on the grades"), card_body(DTOutput(ns("pit_test")))),
          card(card_header("Autocorrelation of the grades"), card_body(DTOutput(ns("pit_acf_test"))))
        )
      ),
      nav_panel(
        "Predictive accuracy",
        p(class = "sl-note pt-3",
          "Log-predictive density and point-forecast error metrics, with the ",
          "realized coverage of the predictive interval."),
        layout_columns(
          col_widths = cols_split(4, 8),
          card(card_header("Log-predictive density"), card_body(DTOutput(ns("lpd")))),
          card(
            card_header("Forecast metrics"),
            card_body(
              layout_columns(
                col_widths = cols_2(),
                sliderInput(ns("fc_ci"), "One-sided tail probability", min = 0.01,
                            max = 0.25, value = 0.10, step = 0.01, ticks = FALSE),
                div(class = "sl-btn-row pt-4",
                    actionButton(ns("run_forecast"), "Compute metrics",
                                 icon = icon("gauge"), class = "btn-primary sl-action"))
              ),
              uiOutput(ns("forecast_cost")),
              DTOutput(ns("forecast"))
            )
          )
        )
      ),
      nav_panel(
        "Information criteria",
        p(class = "sl-note pt-3",
          "Log-likelihood, number of free parameters, AIC and BIC of the current fit."),
        DTOutput(ns("ic"))
      )
    )
  )
}

mod_diagnostics_server <- function(id, model, fitted_data) {
  moduleServer(id, function(input, output, session) {

    lags <- reactive({
      v <- suppressWarnings(as.integer(trimws(strsplit(input$lags %||% "7", ",")[[1]])))
      v <- v[!is.na(v) & v > 0]
      if (length(v) == 0) 7L else sort(unique(v))
    })

    # Re-run on demand, and once automatically after each new fit.
    trigger <- reactive({ list(model(), input$run) })

    autocorr <- reactive({
      req(model())
      trigger()
      isolate({
        if (input$method == "bg" && !HAS_LMTEST) {
          return(structure("The Breusch-Godfrey test needs the lmtest package. Install it or pick another test.",
                           class = "solarr_app_error"))
        }
        res <- lapply(lags(), function(l) {
          out <- safely(solarr::solarModel_test_autocorr(
            model(), method = input$method, type = input$type, lag.max = l, ci = input$ci))
          if (is_app_error(out)) return(NULL)
          out$lag <- l
          out
        })
        res <- res[!vapply(res, is.null, logical(1))]
        if (length(res) == 0) {
          structure("The autocorrelation tests could not be computed on this sample.",
                    class = "solarr_app_error")
        } else {
          out <- dplyr::bind_rows(res)
          out <- dplyr::select(out, lag, target, statistic, p.value, H0, result)
          out[order(out$lag, match(out$target, c(names(RESIDUAL_SERIES),
                                                 paste0(names(RESIDUAL_SERIES), "2")))), ]
        }
      })
    })

    ks <- function(H0) {
      reactive({
        req(model())
        trigger()
        isolate(safely(solarr::solarModel_test_distribution(
          model(), H0 = H0, type = input$type, ci = input$ci,
          min_quantile = input$min_q, max_quantile = input$max_q)))
      })
    }
    ks_gm   <- ks("gm")
    ks_norm <- ks("norm")

    pit_grades <- reactive({
      req(model())
      trigger()
      isolate({
        moments <- model()$moments$conditional
        d <- model()$data
        if (input$type == "train") moments <- moments[d$isTrain & d$weights != 0, ]
        else if (input$type == "test") moments <- moments[!d$isTrain, ]
        safely(solarr::solarModel_PIT(model(), moments)$grade)
      })
    })

    pit_test <- reactive({
      req(model())
      trigger()
      isolate(safely(solarr::solarModel_PIT_test(
        model(), type = input$type, ci = input$ci,
        min_quantile = input$min_q, max_quantile = input$max_q)))
    })

    lpd <- reactive({
      req(model()); trigger()
      isolate(safely(solarr::solarModel_test_LPD(model(), type = input$type)))
    })

    # solarModel_test_forecast() inverts the predictive CDF one day at a time:
    # roughly ten seconds per thousand observations. Never run it on its own.
    forecast_n <- reactive({
      req(model())
      nrow(filter_sample(model()$data, input$type))
    })

    output$forecast_cost <- renderUI({
      n <- forecast_n()
      helpText(sprintf(
        "%s observations in the %s sample: expect around %.0f seconds, during which the app is busy.",
        format(n, big.mark = ","), input$type, n / 100))
    })

    forecast <- eventReactive(input$run_forecast, {
      req(model())
      n <- forecast_n()
      withProgress(message = "Inverting the predictive distribution", value = 0.1, {
        setProgress(0.3, detail = sprintf("%s observations", format(n, big.mark = ",")))
        out <- safely(solarr::solarModel_test_forecast(model(), ci = input$fc_ci,
                                                       type = input$type))
        setProgress(1)
        out
      })
    })

    ic <- reactive({
      req(model()); trigger()
      isolate(safely(solarr::solarModel_AIC_BIC(model())))
    })

    # ---------------------------------------------------------------------- #
    # Scorecard
    # ---------------------------------------------------------------------- #
    output$scorecard <- renderUI({
      req(model())
      box <- function(title, value, ok) {
        theme <- if (is.na(ok)) "secondary" else if (ok) "success" else "danger"
        kpi_box(title, value, theme)
      }
      a <- autocorr(); g <- ks_gm(); pt <- pit_test()

      ac_val <- if (is_app_error(a)) "-" else
        sprintf("%d / %d passed", sum(a$result == "passed"), nrow(a))
      ac_ok <- if (is_app_error(a)) NA else all(a$result == "passed")

      ks_val <- if (is_app_error(g)) "-" else
        sprintf("%d / 12 passed", sum(g$result == "Passed"))
      ks_ok <- if (is_app_error(g)) NA else all(g$result == "Passed")

      pit_val <- if (is_app_error(pt)) "-" else pt$H0
      pit_ok <- if (is_app_error(pt)) NA else pt$H0 == "Non-Rejected"

      l <- lpd()
      lpd_val <- if (is_app_error(l)) "-" else sprintf("%.4f", l$LPD)

      layout_columns(
        col_widths = cols_4(),
        box("Autocorrelation", ac_val, ac_ok),
        box("Mixture fit (KS)", ks_val, ks_ok),
        box("PIT uniformity", pit_val, pit_ok),
        kpi_box("Log-predictive density", lpd_val, "primary")
      )
    })

    # ---------------------------------------------------------------------- #
    # Tables
    # ---------------------------------------------------------------------- #
    render_or_message <- function(x, fun) {
      if (is_app_error(x)) sl_message_table(x) else fun(x)
    }

    output$autocorr <- renderDT({
      render_or_message(autocorr(), function(tab) {
        sl_table(tab, page_length = 15, digits = 5, plain = TRUE,
                 signif_cols = "statistic",
                 colnames = c("Lag", "Series", "Statistic", "p-value",
                              "H0", "Outcome")) |>
          DT::formatRound("p.value", digits = 5) |>
          sl_outcome("result", "passed")
      })
    })

    ks_table <- function(x) {
      render_or_message(x, function(tab) {
        tab <- as.data.frame(tab)
        tab$Month <- MONTH_ABB[tab$Month]
        keep <- intersect(c("Month", "n", "KS", "rejection_lev", "p.value", "result"),
                          names(tab))
        sl_table(tab[, keep], page_length = NULL, digits = 4, ordering = FALSE,
                 round_cols = intersect(c("KS", "rejection_lev", "p.value"), keep),
                 colnames = c("Month", "n", "KS", "Critical", "p-value", "Outcome")) |>
          sl_outcome("result", "Passed")
      })
    }
    output$ks_gm   <- renderDT(ks_table(ks_gm()))
    output$ks_norm <- renderDT(ks_table(ks_norm()))

    output$ks_plot <- renderPlot({
      g <- ks_gm(); nrm <- ks_norm()
      validate(need(!is_app_error(g) && !is_app_error(nrm),
                    "The distribution tests could not be computed on this sample."))
      df <- rbind(
        data.frame(Month = g$Month, KS = g$KS, H0 = "Gaussian mixture",
                   crit = g$rejection_lev),
        data.frame(Month = nrm$Month, KS = nrm$KS, H0 = "Normal",
                   crit = nrm$rejection_lev)
      )
      df$MonthLab <- factor(MONTH_ABB[df$Month], levels = MONTH_ABB)
      ggplot(df, aes(x = MonthLab, y = KS, fill = H0)) +
        geom_col(position = position_dodge(width = 0.75), width = 0.68) +
        geom_hline(aes(yintercept = max(crit)), colour = PAL[["bad"]],
                   linetype = "dashed", linewidth = 0.5) +
        scale_fill_manual(values = c(`Gaussian mixture` = PAL[["main"]],
                                     Normal = PAL[["accent"]])) +
        labs(x = NULL, y = "KS statistic",
             subtitle = "Dashed line: rejection threshold. Bars below it do not reject.") +
        theme_solar()
    })

    output$pit_hist <- renderPlot({
      g <- pit_grades()
      validate(need(!is_app_error(g), as.character(g)))
      gg_pit_hist(g, ci = input$ci)
    })

    output$pit_ecdf <- renderPlot({
      g <- pit_grades()
      validate(need(!is_app_error(g), as.character(g)))
      gg_pit_ecdf(g, ci = input$ci)
    })

    output$pit_test <- renderDT({
      render_or_message(pit_test(), function(tab) {
        tab <- as.data.frame(tab)
        tab$result <- ifelse(tab$H0 == "Non-Rejected", "Passed", "Not-passed")
        sl_table(tab, page_length = NULL, digits = 4, ordering = FALSE,
                 round_cols = intersect(c("KS", "rejection_lev", "p.value"), names(tab))) |>
          sl_outcome("result", "Passed")
      })
    })

    output$pit_acf_test <- renderDT({
      g <- pit_grades()
      render_or_message(g, function(grades) {
        tab <- do.call(rbind, lapply(lags(), function(l) {
          t <- stats::Box.test(grades, lag = l, type = "Ljung-Box")
          data.frame(lag = l, statistic = unname(t$statistic),
                     p.value = t$p.value,
                     H0 = ifelse(t$p.value > input$ci, "Non-Rejected", "Rejected"),
                     result = ifelse(t$p.value > input$ci, "Passed", "Not-passed"))
        }))
        sl_table(tab, page_length = NULL, digits = 5, ordering = FALSE,
                 signif_cols = "statistic",
                 colnames = c("Lag", "Statistic", "p-value", "H0", "Outcome")) |>
          DT::formatRound("p.value", digits = 5) |>
          sl_outcome("result", "Passed")
      })
    })

    output$lpd <- renderDT({
      render_or_message(lpd(), function(tab) {
        sl_table(as.data.frame(tab), page_length = NULL, digits = 5,
                 ordering = FALSE, scroll_x = FALSE, round_cols = "LPD",
                 colnames = c("Sample", "Link", "LPD"))
      })
    })

    output$forecast <- renderDT({
      if (input$run_forecast == 0) {
        return(sl_message_table("Press the button to compute the metrics."))
      }
      render_or_message(forecast(), function(tab) {
        sl_table(as.data.frame(tab), page_length = NULL, digits = 4, ordering = FALSE,
                 round_cols = c("SSE", "RMSE", "MAE", "MAPE", "viol_ci_hi", "viol_ci_lo"),
                 colnames = c("Sample", "SSE", "RMSE", "MAE", "MAPE", "Interval",
                              "Upper viol.", "Lower viol."))
      })
    })

    output$ic <- renderDT({
      render_or_message(ic(), function(tab) {
        tab <- as.data.frame(tab)
        keep <- intersect(c("Model", "p", "q", "r", "s", "L", "k", "n", "AIC", "BIC"),
                          names(tab))
        sl_table(tab[, keep], page_length = NULL, digits = 3, ordering = FALSE,
                 round_cols = intersect(c("L", "AIC", "BIC"), keep))
      })
    })

    list(
      autocorr = autocorr, ks_gm = ks_gm, ks_norm = ks_norm,
      pit_test = pit_test, pit_grades = pit_grades, lpd = lpd,
      forecast = forecast, ic = ic
    )
  })
}
