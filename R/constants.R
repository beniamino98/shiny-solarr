# ---------------------------------------------------------------------------
# Static choices and metadata used to build the interface.
# No package is attached when this file is sourced: keep it to literals.
# ---------------------------------------------------------------------------

APP_TITLE   <- "solarModel"
APP_VERSION <- "0.1.0"

# Link functions accepted by solarTransform (see boundTransform_av_links()).
LINK_CHOICES <- c("invgumbel", "gumbel", "norm", "logis", "identity")

CLEARSKY_METHODS    <- c(OLS = "OLS", RLS = "RLS", CLS = "CLS")
CLEARSKY_H0_METHODS <- c(Spencer = "spencer", Cooper = "cooper")
MIXTURE_METHODS     <- c(mclust = "mclust", mixtools = "mixtools")

SAMPLE_TYPES <- c(`Full sample` = "full", `Train only` = "train", `Test only` = "test")

AUTOCORR_METHODS <- c(
  `Breusch-Godfrey` = "bg",
  `Ljung-Box`       = "lb",
  `Box-Pierce`      = "bp"
)

MONTH_LABELS <- c("January", "February", "March", "April", "May", "June",
                  "July", "August", "September", "October", "November", "December")

MONTH_ABB <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
               "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

# --------------------------------------------------------------------------- #
# Residual series produced along the estimation pipeline.
#
# `expected` / `expected_sq` mirror the reference outcomes hard-coded in
# solarr::solarModel_test_autocorr(): what an adequately specified model should
# return for the series itself and for its square.
# `reference` is the distribution the series is compared against in the
# residual diagnostics.
# --------------------------------------------------------------------------- #
RESIDUAL_SERIES <- list(
  Yt_tilde = list(
    label       = "Yt_tilde  -  deseasonalized transformed series",
    short       = "Yt_tilde",
    stage       = "Step 4: Yt minus its seasonal mean",
    formula     = "Yt_tilde = Yt - Yt_bar",
    expected    = "Rejected",
    expected_sq = "Rejected",
    reference   = "none",
    note        = "Input of the ARMA model. Autocorrelation is expected here: it is the signal the ARMA component has to absorb."
  ),
  eps = list(
    label       = "eps  -  ARMA residuals",
    short       = "eps",
    stage       = "Step 5: residuals of the ARMA(p, q) mean model",
    formula     = "eps = Yt_tilde - Yt_tilde_hat",
    expected    = "Not-rejected",
    expected_sq = "Rejected",
    reference   = "normal",
    note        = "Serial correlation should be gone, but the squares stay autocorrelated: seasonal heteroskedasticity is still in."
  ),
  eps_tilde = list(
    label       = "eps_tilde  -  seasonally standardized residuals",
    short       = "eps_tilde",
    stage       = "Step 6: eps divided by the seasonal standard deviation",
    formula     = "eps_tilde = eps / sigma_bar",
    expected    = "Not-rejected",
    expected_sq = "Rejected",
    reference   = "normal",
    note        = "Seasonal variance removed. Remaining squared autocorrelation is the GARCH effect."
  ),
  u_tilde = list(
    label       = "u_tilde  -  GARCH standardized residuals",
    short       = "u_tilde",
    stage       = "Step 7: eps_tilde divided by the GARCH conditional sigma",
    formula     = "u_tilde = eps_tilde / sigma",
    expected    = "Not-rejected",
    expected_sq = "Not-rejected",
    reference   = "mixture",
    note        = "The series the monthly Gaussian mixture is fitted on. Both levels and squares should now be free of autocorrelation."
  ),
  z_tilde = list(
    label       = "z_tilde  -  mixture standardized residuals",
    short       = "z_tilde",
    stage       = "Step 8: u_tilde centred and scaled by its monthly mixture moments",
    formula     = "z_tilde = (u_tilde - mean_m) / sqrt(variance_m)",
    expected    = "Not-rejected",
    expected_sq = "Not-rejected",
    reference   = "mixture",
    note        = "Final residual of the model. Mean zero and unit variance by construction, month by month."
  )
)

RESIDUAL_CHOICES <- stats::setNames(
  names(RESIDUAL_SERIES),
  vapply(RESIDUAL_SERIES, function(x) x$label, character(1))
)


# --------------------------------------------------------------------------- #
# Deterministic components the fit produces, one per day of the year.
# --------------------------------------------------------------------------- #
SEASONAL_COMPONENTS <- list(
  Ct = list(
    label  = "Clear sky",
    tab    = "Clear sky",
    symbol = "C_t",
    unit   = "kWh/m2",
    note   = "Seasonal ceiling fitted on the CAMS clear-sky series. Every risk driver is measured relative to it."
  ),
  GHI_bar = list(
    label  = "Seasonal mean radiation",
    tab    = "Mean radiation",
    symbol = "GHI_bar",
    unit   = "kWh/m2",
    note   = "The seasonal mean of the transformed series carried back to the radiation scale."
  ),
  Yt_bar = list(
    label  = "Seasonal mean",
    tab    = "Seasonal mean",
    symbol = "Yt_bar",
    unit   = "transformed scale",
    note   = "Fitted seasonal mean of Yt. Subtracting it produces Yt_tilde, the series the ARMA model works on."
  ),
  sigma_bar = list(
    label  = "Seasonal volatility",
    tab    = "Volatility",
    symbol = "sigma_bar",
    unit   = "transformed scale",
    note   = "Seasonal standard deviation of the ARMA residuals. Dividing by it produces eps_tilde, the GARCH input."
  )
)

# --------------------------------------------------------------------------- #
# Monthly mixture parameters, grouped the way they are read.
# --------------------------------------------------------------------------- #
MIXTURE_GROUPS <- list(
  means = list(
    label   = "Means",
    columns = c(mu1 = "mu1  (cloudy)", mu2 = "mu2  (sunny)"),
    ylab    = "Component mean",
    zero    = TRUE,
    note    = "Centre of each regime. The cloudy component sits below zero, the sunny one above."
  ),
  sd = list(
    label   = "Std. deviations",
    columns = c(sd1 = "sd1  (cloudy)", sd2 = "sd2  (sunny)"),
    ylab    = "Component standard deviation",
    zero    = FALSE,
    note    = "Spread of each regime. The cloudy component is consistently the wider of the two."
  ),
  probs = list(
    label   = "Probabilities",
    columns = c(p1 = "p1  (cloudy)", p2 = "p2  (sunny)"),
    ylab    = "Prior probability",
    zero    = FALSE,
    note    = "Weight of each regime, month by month. Together they sum to one."
  )
)


# --------------------------------------------------------------------------- #
# Responsive column recipes.
#
# layout_columns() keeps a fixed grid all the way down unless the widths are
# given per breakpoint, which squashes four value boxes into unreadable columns
# on a narrow window. These are the three shapes the app uses.
# --------------------------------------------------------------------------- #
# Each breakpoint must carry a vector of the same length as the row: mixing a
# scalar with a vector makes bslib size the grid from the scalar, so a
# `sm = 6` next to `lg = c(6, 3, 3)` yields a six-column grid and the first
# item swallows the whole row.
# Four across only from xl: between lg and xl the 340px sidebar leaves each
# box too narrow for its own label.
cols_4 <- function() bslib::breakpoints(sm = rep(6, 4), xl = rep(3, 4))
cols_2 <- function() bslib::breakpoints(sm = rep(12, 2), lg = rep(6, 2))
cols_split <- function(wide, narrow) {
  bslib::breakpoints(sm = c(12, 12), lg = c(wide, narrow))
}
cols_row <- function(...) {
  w <- c(...)
  bslib::breakpoints(sm = rep(6, length(w)), md = rep(6, length(w)), lg = w)
}
