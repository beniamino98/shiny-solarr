# ---------------------------------------------------------------------------
# Building, fitting and unpacking a solarModel.
# ---------------------------------------------------------------------------

#' Build a solarModel_spec from the values collected in the sidebar
#'
#' All `set_*()` calls have to happen before `$specification()`: the latter
#' reads `clearsky_threshold` and pushes the latitude into the clear-sky model.
build_spec <- function(cfg) {
  spec <- solarr::solarModel_spec$new()

  spec$set_params(
    stochastic_clearsky = cfg$stochastic_clearsky,
    clearsky_threshold  = cfg$clearsky_threshold,
    quiet               = TRUE
  )
  spec$set_transform(
    min_pos   = cfg$tr_min_pos,
    max_pos   = cfg$tr_max_pos,
    link      = cfg$tr_link,
    delta     = cfg$tr_delta,
    threshold = cfg$tr_threshold
  )
  spec$set_clearsky(
    spec = solarr::seasonalClearsky_spec(
      method            = cfg$cs_method,
      order             = cfg$cs_order,
      order_H0          = cfg$cs_order_H0,
      method_H0         = cfg$cs_method_H0,
      period            = cfg$cs_period,
      include.intercept = cfg$cs_intercept,
      include.trend     = cfg$cs_trend
    ),
    control = solarr::control_seasonalClearsky(quiet = TRUE)
  )
  spec$set_seasonal.mean(
    order             = cfg$sm_order,
    period            = cfg$sm_period,
    include.trend     = cfg$sm_trend,
    include.intercept = cfg$sm_intercept,
    monthly.mean      = cfg$sm_monthly
  )
  spec$set_mean.model(
    arOrder           = cfg$ar_order,
    maOrder           = cfg$ma_order,
    include.intercept = cfg$arma_intercept
  )
  spec$set_seasonal.variance(
    order         = cfg$sv_order,
    period        = cfg$sv_period,
    include.trend = cfg$sv_trend,
    correction    = cfg$sv_correction,
    monthly.mean  = cfg$sv_monthly
  )
  spec$set_variance.model(
    archOrder  = cfg$arch_order,
    garchOrder = cfg$garch_order
  )
  spec$set_mixture.model(
    abstol            = cfg$mx_abstol,
    match.expectation = cfg$mx_match_exp,
    match.variance    = cfg$mx_match_var,
    match.empiric     = cfg$mx_match_emp,
    method            = cfg$mx_method,
    maxit             = cfg$mx_maxit,
    maxrestarts       = cfg$mx_maxrestarts
  )

  spec$specification(
    place    = cfg$place,
    target   = cfg$target,
    min_date = cfg$sample_from,
    max_date = cfg$sample_to,
    from     = cfg$train_from,
    to       = cfg$train_to
  )
  spec
}

#' Fit a model, capturing console output and errors instead of crashing the app
fit_solar_model <- function(spec) {
  model <- solarr::solarModel$new(spec)
  log <- utils::capture.output(
    result <- try(suppressWarnings(model$fit()), silent = TRUE),
    type = "message"
  )
  if (inherits(result, "try-error")) {
    return(list(ok = FALSE, model = NULL, log = log,
                error = conditionMessage(attr(result, "condition"))))
  }
  list(ok = TRUE, model = model, log = log, error = NULL)
}

is_solar_model <- function(x) inherits(x, "solarModel")

#' Full fitted data frame with every residual series, z_tilde included
#'
#' `z_tilde` is not stored by the model: it is rebuilt here exactly as in
#' solarr::solarModel_test_autocorr(), standardizing u_tilde by the monthly
#' mixture moments.
model_frame <- function(model) {
  data <- model$data
  mom  <- dplyr::select(model$spec$mixture.model$moments, Month, mean, variance)
  data <- dplyr::left_join(data, mom, by = "Month")
  data$z_tilde <- (data$u_tilde - data$mean) / sqrt(data$variance)
  data$sample  <- ifelse(data$isTrain & data$weights != 0, "train", "test")
  data$MonthLab <- factor(MONTH_ABB[data$Month], levels = MONTH_ABB)
  data
}

#' Subset a fitted data frame the same way the solarr test functions do
filter_sample <- function(data, type = c("full", "train", "test")) {
  type <- match.arg(type)
  if (type == "train") data[data$isTrain & data$weights != 0, ]
  else if (type == "test") data[!data$isTrain, ]
  else data
}

#' Monthly Gaussian mixture parameters as a plain data frame
mixture_params <- function(model) {
  as.data.frame(model$spec$mixture.model$coefficients)
}

#' Long, tidy table of the point estimates and their standard errors
#'
#' `$coefficients` and `$std.errors` are lists of one-row tibbles sharing the
#' same names and columns, so they can be flattened in parallel.
tidy_coefficients <- function(model) {
  est <- model$coefficients
  se  <- model$spec$std.errors
  components <- setdiff(names(est), "location")

  rows <- lapply(components, function(cmp) {
    e <- est[[cmp]]
    s <- if (!is.null(se[[cmp]])) se[[cmp]] else NULL
    pars <- names(e)
    data.frame(
      component  = cmp,
      parameter  = pars,
      estimate   = as.numeric(unlist(e[1, pars])),
      std.error  = if (is.null(s)) NA_real_ else as.numeric(unlist(s[1, pars])),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)

  # `spec$std.errors$params` repeats alpha and beta instead of their standard
  # errors: the transformation bounds are sample extrema of Xt, not maximum
  # likelihood estimates, so no standard error exists. Blank them out rather
  # than report a t-statistic that is 1 by construction.
  out$std.error[out$component == "params"] <- NA_real_

  out$statistic <- out$estimate / out$std.error
  out$p.value   <- 2 * stats::pnorm(-abs(out$statistic))
  rownames(out) <- NULL
  out
}

#' One-line-per-metric summary of the fit
model_summary <- function(model) {
  ic <- try(solarr::solarModel_AIC_BIC(model), silent = TRUE)
  dates <- model$spec$dates
  data.frame(
    metric = c("Location", "Model", "Target", "Coordinates",
               "Sample", "Observations", "Train window", "Test window",
               "Log-likelihood", "Parameters", "AIC", "BIC"),
    value = c(
      model$spec$place,
      model$spec$model_name,
      model$spec$target,
      sprintf("lat %s / lon %s / alt %s m",
              model$spec$coords$lat, model$spec$coords$lon, model$spec$coords$alt),
      sprintf("%s to %s", dates$data$from, dates$data$to),
      format(dates$data$nobs, big.mark = ","),
      sprintf("%s to %s (%s obs, %.1f%%)", dates$train$from, dates$train$to,
              dates$train$nobs, dates$train$perc * 100),
      sprintf("%s to %s (%s obs, %.1f%%)", dates$test$from, dates$test$to,
              dates$test$nobs, dates$test$perc * 100),
      format(model$loglik, digits = 8),
      if (inherits(ic, "try-error")) NA_character_ else as.character(ic$k),
      if (inherits(ic, "try-error")) NA_character_ else format(ic$AIC, digits = 8),
      if (inherits(ic, "try-error")) NA_character_ else format(ic$BIC, digits = 8)
    ),
    stringsAsFactors = FALSE
  )
}

#' Evaluate an expression, returning the condition message on failure
#'
#' Diagnostic tests can legitimately fail (a month with too few observations in
#' the test sample, a missing optional package): show why instead of blanking
#' the panel.
safely <- function(expr) {
  out <- try(suppressWarnings(expr), silent = TRUE)
  if (inherits(out, "try-error")) {
    structure(conditionMessage(attr(out, "condition")), class = "solarr_app_error")
  } else {
    out
  }
}

is_app_error <- function(x) inherits(x, "solarr_app_error")

#' EM implementation actually used by the monthly mixtures
#'
#' `solarMixture` keeps the setting on the per-month `gaussianMixture` objects
#' rather than exposing it directly.
mixture_method <- function(spec) {
  m <- try(spec$mixture.model$model[[1]]$method, silent = TRUE)
  if (inherits(m, "try-error") || length(m) != 1 || is.na(m)) "mclust" else as.character(m)
}
