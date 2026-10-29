# ---------------------------------------------------------------------------
# Tab "Export": take the fitted object and the tables out of the app.
# ---------------------------------------------------------------------------

mod_export_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = cols_2(),
      card(
        card_header("Fitted model"),
        card_body(
          p(class = "sl-note",
            "The .rds holds the complete solarModel R6 object. Read it back with ",
            tags$code("readRDS()"), " in an R session, or load it from the sidebar ",
            "to reopen it here."),
          div(class = "sl-btn-row",
              downloadButton(ns("dl_model"), "Download model (.rds)",
                             class = "btn-primary sl-action")),
          hr(),
          div(class = "sl-subhead", "Reproduce this fit in a script"),
          verbatimTextOutput(ns("code"))
        )
      ),
      card(
        card_header("Tables"),
        card_body(
          p(class = "sl-note", "Comma separated files, one per table shown in the app."),
          div(class = "sl-downloads",
              downloadButton(ns("dl_coefs"), "Coefficients",
                             class = "btn-outline-secondary sl-action"),
              downloadButton(ns("dl_data"), "Fitted data and residuals",
                             class = "btn-outline-secondary sl-action"),
              downloadButton(ns("dl_autocorr"), "Autocorrelation tests",
                             class = "btn-outline-secondary sl-action"),
              downloadButton(ns("dl_ks"), "Distribution tests",
                             class = "btn-outline-secondary sl-action"),
              downloadButton(ns("dl_pit"), "PIT grades",
                             class = "btn-outline-secondary sl-action")
          )
        )
      )
    ),
    card(
      card_header("Estimation log"),
      card_body(verbatimTextOutput(ns("log")))
    )
  )
}

mod_export_server <- function(id, model, fitted_data, estimate, diagnostics, fit_log) {
  moduleServer(id, function(input, output, session) {

    stamp <- function(what, ext) {
      m <- model()
      place <- if (is.null(m)) "model" else gsub("[^A-Za-z0-9]+", "-", m$spec$place)
      sprintf("solarr-%s-%s-%s.%s", place, what, format(Sys.Date(), "%Y%m%d"), ext)
    }

    write_csv_or_note <- function(x, file) {
      if (is.null(x) || is_app_error(x)) {
        utils::write.csv(data.frame(message = as.character(x %||% "not available")),
                         file, row.names = FALSE)
      } else {
        utils::write.csv(as.data.frame(x), file, row.names = FALSE)
      }
    }

    output$dl_model <- downloadHandler(
      filename = function() stamp("model", "rds"),
      content = function(file) { req(model()); saveRDS(model(), file) }
    )

    output$dl_coefs <- downloadHandler(
      filename = function() stamp("coefficients", "csv"),
      content = function(file) { req(model()); write_csv_or_note(tidy_coefficients(model()), file) }
    )

    output$dl_data <- downloadHandler(
      filename = function() stamp("fitted-data", "csv"),
      content = function(file) { req(fitted_data()); write_csv_or_note(fitted_data(), file) }
    )

    output$dl_autocorr <- downloadHandler(
      filename = function() stamp("autocorrelation-tests", "csv"),
      content = function(file) write_csv_or_note(diagnostics$autocorr(), file)
    )

    output$dl_ks <- downloadHandler(
      filename = function() stamp("distribution-tests", "csv"),
      content = function(file) {
        gm <- diagnostics$ks_gm(); nm <- diagnostics$ks_norm()
        if (is_app_error(gm) || is_app_error(nm)) {
          write_csv_or_note(gm, file)
        } else {
          write_csv_or_note(dplyr::bind_rows(as.data.frame(gm), as.data.frame(nm)), file)
        }
      }
    )

    output$dl_pit <- downloadHandler(
      filename = function() stamp("pit-grades", "csv"),
      content = function(file) {
        g <- diagnostics$pit_grades()
        if (is_app_error(g)) write_csv_or_note(g, file)
        else write_csv_or_note(data.frame(grade = g), file)
      }
    )

    output$code <- renderPrint({
      m <- model()
      validate(need(!is.null(m), "Fit a model first."))
      spec <- m$spec
      d <- spec$dates
      cs <- spec$seasonal_model_Ct
      cat(sprintf(
'library(solarr)

spec <- solarModel_spec$new()
spec$set_params(stochastic_clearsky = %s, clearsky_threshold = %s)
spec$set_transform(link = "%s", min_pos = %s, max_pos = %s,
                   delta = %s, threshold = %s)
spec$set_seasonal.mean(order = %s, period = %s)
spec$set_mean.model(arOrder = %s, maOrder = %s)
spec$set_seasonal.variance(order = %s, period = %s)
spec$set_variance.model(archOrder = %s, garchOrder = %s)
spec$set_mixture.model(method = "%s")
spec$specification("%s", target = "%s",
                   min_date = "%s", max_date = "%s",
                   from = "%s", to = "%s")

model <- solarModel$new(spec)
model$fit()

solarModel_tests(model, lags = 7, method = "bg", type = "full")
solarModel_PIT_test(model)
solarModel_AIC_BIC(model)
',
        spec$stochastic_clearsky, spec$clearsky_threshold,
        spec$transform$link,
        spec$transform$control$min_pos, spec$transform$control$max_pos,
        spec$transform$control$delta, spec$transform$control$threshold,
        spec$seasonal.mean$order, spec$seasonal.mean$period,
        spec$mean.model$arOrder, spec$mean.model$maOrder,
        spec$seasonal.variance$order, spec$seasonal.variance$period,
        spec$variance.model$archOrder, spec$variance.model$garchOrder,
        mixture_method(spec),
        spec$place, spec$target,
        d$data$from, d$data$to, d$train$from, d$train$to
      ))
    })

    output$log <- renderPrint({
      lg <- fit_log()
      if (is.null(lg) || length(lg) == 0) cat("Nothing reported during the last fit.")
      else cat(paste(lg, collapse = "\n"))
    })
  })
}
