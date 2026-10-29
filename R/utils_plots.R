# ---------------------------------------------------------------------------
# ggplot2 helpers. Every function returns a plot object so it can be reused by
# the download handlers.
# ---------------------------------------------------------------------------

PAL <- c(
  main   = "#1f4e79",
  accent = "#e07b39",
  ok     = "#2e7d32",
  bad    = "#c62828",
  grey   = "#9e9e9e",
  dw     = "#2e7d32",
  up     = "#e07b39"
)

theme_solar <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(colour = "#eef1f5", linewidth = 0.4),
      panel.border      = element_rect(colour = "#dfe4ea", linewidth = 0.5),
      axis.ticks        = element_line(colour = "#c9d1da", linewidth = 0.3),
      axis.text         = element_text(colour = "#6b7885", size = base_size - 2.5),
      axis.title        = element_text(colour = "#6b7885", size = base_size - 2),
      strip.background  = element_rect(fill = "#f2f5f8", colour = "#dfe4ea"),
      strip.text        = element_text(face = "bold", size = base_size - 2,
                                       colour = "#1c2733",
                                       margin = margin(3.5, 3.5, 3.5, 3.5)),
      legend.position   = "top",
      legend.justification = "left",
      legend.title      = element_blank(),
      legend.text       = element_text(size = base_size - 2.5),
      legend.key.height = unit(0.8, "lines"),
      legend.margin     = margin(0, 0, 2, 0),
      plot.title        = element_text(face = "bold", size = base_size,
                                       colour = "#1c2733"),
      plot.subtitle     = element_text(colour = "#6b7885", size = base_size - 2.5),
      plot.title.position = "plot",
      plot.margin       = margin(4, 8, 2, 2)
    )
}

empty_plot <- function(msg) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, size = 4.2, colour = "grey30") +
    theme_void()
}

# --------------------------------------------------------------------------- #
# Reference densities
# --------------------------------------------------------------------------- #

#' Density of the monthly Gaussian mixture, pooled over the months present
#'
#' With a single month this is the fitted mixture. Pooling several months the
#' correct reference is the mixture of the monthly mixtures, weighted by the
#' number of observations each month contributes.
mixture_density <- function(model, x, months, weights = NULL, series = "u_tilde") {
  params <- mixture_params(model)
  params <- params[params$Month %in% months, , drop = FALSE]
  if (nrow(params) == 0) return(rep(NA_real_, length(x)))
  if (series == "z_tilde") {
    # z_tilde lives on the standardized scale: move the component parameters
    # there as well, month by month.
    mom <- model$spec$mixture.model$moments
    mom <- mom[match(params$Month, mom$Month), ]
    shift <- mom$mean
    scale <- sqrt(mom$variance)
    params$mu1 <- (params$mu1 - shift) / scale
    params$mu2 <- (params$mu2 - shift) / scale
    params$sd1 <- params$sd1 / scale
    params$sd2 <- params$sd2 / scale
  }
  if (is.null(weights)) {
    w <- rep(1 / nrow(params), nrow(params))
  } else {
    w <- weights[as.character(params$Month)]
    w[is.na(w)] <- 0
    w <- w / sum(w)
  }
  dens <- vapply(seq_len(nrow(params)), function(i) {
    p <- params[i, ]
    w[i] * solarr::dmixnorm(x, mean = c(p$mu1, p$mu2), sd = c(p$sd1, p$sd2),
                            alpha = c(p$p1, p$p2))
  }, numeric(length(x)))
  if (is.null(dim(dens))) sum(dens) else rowSums(dens)
}

#' PIT grades of a residual series against its own monthly mixture
#'
#' Each observation is mapped through the mixture CDF of its month. Under a
#' correct specification the grades are uniform, whatever the month mix.
mixture_pit <- function(model, data, series = "u_tilde") {
  params <- mixture_params(model)
  grades <- rep(NA_real_, nrow(data))
  for (m in unique(data$Month)) {
    p <- params[params$Month == m, ]
    if (nrow(p) == 0) next
    idx <- which(data$Month == m)
    x <- data[[series]][idx]
    if (series == "z_tilde") {
      # z_tilde is u_tilde recentred/rescaled: undo it before using the mixture
      mom <- model$spec$mixture.model$moments
      mom <- mom[mom$Month == m, ]
      x <- x * sqrt(mom$variance) + mom$mean
    }
    grades[idx] <- solarr::pmixnorm(x, mean = c(p$mu1, p$mu2),
                                    sd = c(p$sd1, p$sd2), alpha = c(p$p1, p$p2))
  }
  grades
}

# --------------------------------------------------------------------------- #
# Plots
# --------------------------------------------------------------------------- #

gg_series <- function(data, y, title = NULL, ylab = NULL, colour_by_sample = TRUE) {
  df <- data.frame(date = data$date, value = data[[y]], sample = data$sample)
  df <- df[is.finite(df$value), ]
  if (nrow(df) == 0) return(empty_plot("No observations to plot"))
  p <- ggplot(df, aes(x = date, y = value))
  if (colour_by_sample && length(unique(df$sample)) > 1) {
    p <- p + geom_line(aes(colour = sample), linewidth = 0.25) +
      scale_colour_manual(values = c(train = PAL[["main"]], test = PAL[["accent"]]))
  } else {
    p <- p + geom_line(linewidth = 0.25, colour = PAL[["main"]])
  }
  p + geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
    labs(title = title, x = NULL, y = ylab %||% y) +
    theme_solar()
}

gg_acf <- function(x, lag_max = 60, ci = 0.05, partial = FALSE,
                   title = NULL, squared = FALSE) {
  x <- x[is.finite(x)]
  if (squared) x <- x^2
  if (length(x) < 10) return(empty_plot("Not enough observations"))
  a <- if (partial) stats::pacf(x, lag.max = lag_max, plot = FALSE)
       else stats::acf(x, lag.max = lag_max, plot = FALSE)
  vals <- as.numeric(a$acf)
  lags <- as.numeric(a$lag)
  if (!partial) { vals <- vals[-1]; lags <- lags[-1] }
  bound <- stats::qnorm(1 - ci / 2) / sqrt(a$n.used)
  df <- data.frame(lag = lags, value = vals)
  df$outside <- abs(df$value) > bound

  ggplot(df, aes(x = lag, y = value)) +
    geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.3) +
    geom_hline(yintercept = c(-bound, bound), colour = PAL[["bad"]],
               linetype = "dashed", linewidth = 0.4) +
    geom_segment(aes(xend = lag, yend = 0, colour = outside), linewidth = 0.4) +
    geom_point(aes(colour = outside), size = 0.7) +
    scale_colour_manual(values = c(`FALSE` = PAL[["main"]], `TRUE` = PAL[["bad"]]),
                        guide = "none") +
    labs(title = title,
         subtitle = sprintf("dashed band: +/- %.3f  (%.0f%% under white noise)",
                            bound, (1 - ci) * 100),
         x = "Lag", y = if (partial) "PACF" else "ACF") +
    theme_solar()
}

gg_hist_density <- function(x, bins = 40, reference = c("none", "normal", "mixture"),
                            model = NULL, months = 1:12, weights = NULL,
                            series = "u_tilde", title = NULL, xlab = NULL) {
  reference <- match.arg(reference)
  x <- x[is.finite(x)]
  if (length(x) < 10) return(empty_plot("Not enough observations"))
  df <- data.frame(x = x)
  grid <- seq(min(x), max(x), length.out = 512)

  p <- ggplot(df, aes(x = x)) +
    geom_histogram(aes(y = after_stat(density)), bins = bins,
                   fill = "grey85", colour = "white", linewidth = 0.2) +
    geom_density(aes(colour = "Empirical"), linewidth = 0.7)

  if (reference == "normal") {
    ref <- data.frame(x = grid, y = stats::dnorm(grid, mean(x), stats::sd(x)))
    p <- p + geom_line(data = ref, aes(x = x, y = y, colour = "Normal"), linewidth = 0.7)
  } else if (reference == "mixture" && !is.null(model)) {
    y <- mixture_density(model, grid, months, weights, series = series)
    ref <- data.frame(x = grid, y = y)
    p <- p + geom_line(data = ref, aes(x = x, y = y, colour = "Gaussian mixture"),
                       linewidth = 0.7)
  }

  p + scale_colour_manual(values = c(Empirical = PAL[["main"]],
                                     Normal = PAL[["accent"]],
                                     `Gaussian mixture` = PAL[["bad"]])) +
    labs(title = title, x = xlab, y = "Density") +
    theme_solar()
}

#' QQ-plot against the normal or against the fitted monthly mixture
#'
#' For the mixture the comparison goes through the PIT: each point is mapped by
#' the CDF of its own month and then back through qnorm(), so a straight line
#' still means "the reference distribution fits".
gg_qq <- function(x, reference = c("normal", "mixture"), model = NULL,
                  data = NULL, series = NULL, title = NULL) {
  reference <- match.arg(reference)
  if (reference == "mixture") {
    grades <- mixture_pit(model, data, series)
    grades <- grades[is.finite(grades)]
    grades <- pmin(pmax(grades, 1e-8), 1 - 1e-8)
    sample_q <- sort(stats::qnorm(grades))
    ref_lab <- "monthly Gaussian mixture (via PIT)"
  } else {
    x <- x[is.finite(x)]
    sample_q <- sort((x - mean(x)) / stats::sd(x))
    ref_lab <- "standard normal"
  }
  n <- length(sample_q)
  if (n < 10) return(empty_plot("Not enough observations"))
  df <- data.frame(theoretical = stats::qnorm(stats::ppoints(n)), sample = sample_q)

  ggplot(df, aes(x = theoretical, y = sample)) +
    geom_abline(slope = 1, intercept = 0, colour = PAL[["bad"]], linewidth = 0.5) +
    geom_point(size = 0.6, colour = PAL[["main"]], alpha = 0.6) +
    labs(title = title, subtitle = paste("Reference:", ref_lab),
         x = "Theoretical quantiles", y = "Sample quantiles") +
    theme_solar()
}

gg_monthly_box <- function(data, y, title = NULL) {
  df <- data.frame(month = data$MonthLab, value = data[[y]])
  df <- df[is.finite(df$value), ]
  ggplot(df, aes(x = month, y = value)) +
    geom_hline(yintercept = 0, colour = "grey40", linewidth = 0.3) +
    geom_boxplot(fill = "grey90", colour = PAL[["main"]], outlier.size = 0.4,
                 outlier.alpha = 0.4, linewidth = 0.35) +
    labs(title = title, x = NULL, y = y) +
    theme_solar()
}

gg_pit_hist <- function(grades, bins = 20, title = NULL, ci = 0.05) {
  grades <- grades[is.finite(grades)]
  if (length(grades) < 10) return(empty_plot("Not enough observations"))
  n <- length(grades)
  # Pointwise band for a bin count under uniformity (binomial normal approx.)
  p <- 1 / bins
  se <- sqrt(p * (1 - p) / n) * bins   # on the density scale
  band <- stats::qnorm(1 - ci / 2) * se

  ggplot(data.frame(u = grades), aes(x = u)) +
    geom_histogram(aes(y = after_stat(density)), bins = bins, boundary = 0,
                   fill = "grey85", colour = "white", linewidth = 0.2) +
    geom_hline(yintercept = 1, colour = PAL[["main"]], linewidth = 0.5) +
    geom_hline(yintercept = c(1 - band, 1 + band), colour = PAL[["bad"]],
               linetype = "dashed", linewidth = 0.4) +
    labs(title = title,
         subtitle = "Flat at 1 under a correctly specified predictive distribution",
         x = "PIT grade", y = "Density") +
    theme_solar()
}

gg_pit_ecdf <- function(grades, title = NULL, ci = 0.05) {
  grades <- grades[is.finite(grades)]
  if (length(grades) < 10) return(empty_plot("Not enough observations"))
  n <- length(grades)
  band <- sqrt(-0.5 * log(ci / 2)) / sqrt(n)   # Kolmogorov-Smirnov band
  grid <- seq(0, 1, length.out = 512)

  ggplot() +
    geom_ribbon(data = data.frame(x = grid,
                                  lo = pmax(grid - band, 0),
                                  hi = pmin(grid + band, 1)),
                aes(x = x, ymin = lo, ymax = hi), fill = PAL[["bad"]], alpha = 0.12) +
    stat_ecdf(data = data.frame(u = grades), aes(x = u), geom = "step",
              colour = PAL[["main"]], linewidth = 0.6) +
    geom_abline(slope = 1, intercept = 0, colour = "grey40", linewidth = 0.4) +
    labs(title = title,
         subtitle = sprintf("shaded: %.0f%% Kolmogorov-Smirnov band", (1 - ci) * 100),
         x = "PIT grade", y = "Empirical CDF") +
    theme_solar()
}

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Compact value box for the KPI rows
#'
#' The unit rides along with the number instead of padding out the title, which
#' keeps both lines short enough to survive a narrow column.
kpi_box <- function(title, value, theme = "secondary", unit = NULL) {
  body <- span(
    class = "sl-value",
    value,
    if (!is.null(unit)) span(class = "sl-unit", unit)
  )
  value_box(title = title, value = body, theme = theme,
            max_height = "104px", fill = FALSE)
}

#' ggplotly with a horizontal, untitled legend
plotly_clean <- function(p) {
  ggplotly(p) |>
    plotly::layout(legend = list(orientation = "h", x = 0, y = 1.12,
                                 title = list(text = "")))
}


# --------------------------------------------------------------------------- #
# Deterministic solar geometry
# --------------------------------------------------------------------------- #

#' Month breaks for a day-of-year axis
month_scale <- function() {
  scale_x_date(date_breaks = "1 month", date_labels = "%b", expand = expansion(0.01))
}

#' Mark the solstices and the equinoxes
solar_marks <- function(data) {
  key <- data[format(data$date, "%m-%d") %in%
                c("03-20", "06-21", "09-22", "12-21"), ]
  geom_vline(data = key, aes(xintercept = date), colour = "#c9d1da",
             linetype = "dashed", linewidth = 0.4)
}

gg_solar_curve <- function(data, variable, title = NULL, ylab = NULL) {
  df <- data.frame(date = data$date, value = data[[variable]])
  peak <- df[which.max(df$value), ]
  low  <- df[which.min(df$value), ]

  ggplot(df, aes(x = date, y = value)) +
    solar_marks(data) +
    geom_hline(yintercept = 0, colour = "#c9d1da", linewidth = 0.4) +
    geom_line(colour = PAL[["main"]], linewidth = 0.8) +
    geom_point(data = rbind(peak, low), colour = PAL[["accent"]], size = 2.2) +
    geom_text(data = rbind(peak, low),
              aes(label = sprintf("%.2f", value)), vjust = -0.9, hjust = 0.5,
              size = 3.1, colour = PAL[["accent"]]) +
    month_scale() +
    scale_y_continuous(expand = expansion(0.08)) +
    labs(title = title,
         subtitle = "Dashed lines: equinoxes and solstices",
         x = NULL, y = ylab) +
    theme_solar()
}

gg_solar_all <- function(data) {
  vars <- names(SOLAR_VARS)
  labs_ <- vapply(SOLAR_VARS, function(x) sprintf("%s  [%s]", x$label, x$unit),
                  character(1))
  df <- do.call(rbind, lapply(vars, function(v) {
    data.frame(date = data$date, value = data[[v]], panel = labs_[[v]])
  }))
  df$panel <- factor(df$panel, levels = labs_)

  ggplot(df, aes(x = date, y = value)) +
    geom_line(colour = PAL[["main"]], linewidth = 0.65) +
    facet_wrap(~panel, ncol = 2, scales = "free_y") +
    month_scale() +
    labs(x = NULL, y = NULL) +
    theme_solar()
}

gg_sun_position <- function(data) {
  df <- data[data$altitude > 0, ]
  if (nrow(df) == 0) return(empty_plot("The sun never rises on this date"))
  long <- rbind(
    data.frame(clocktime = df$clocktime, value = df$altitude,
               panel = "Altitude above the horizon [deg]"),
    data.frame(clocktime = df$clocktime, value = df$azimuth,
               panel = "Azimuth, 0 = south, + = west [deg]")
  )

  # Solar noon is where the hour angle crosses zero.
  noon <- solar_event_time(data, 0)

  p <- ggplot(long, aes(x = clocktime, y = value)) +
    geom_hline(yintercept = 0, colour = "#c9d1da", linewidth = 0.4)
  if (is.finite(noon)) {
    p <- p +
      geom_vline(xintercept = noon, colour = PAL[["accent"]],
                 linetype = "dashed", linewidth = 0.45)
  }
  p + geom_line(colour = PAL[["main"]], linewidth = 0.8) +
    facet_wrap(~panel, ncol = 1, scales = "free_y") +
    scale_x_datetime(date_labels = "%H:%M") +
    labs(x = NULL, y = NULL,
         subtitle = if (is.finite(noon))
           paste("Dashed line: solar noon,", format(noon, "%H:%M")) else NULL) +
    theme_solar()
}

gg_sun_irradiance <- function(data, tilted = FALSE) {
  df <- data[data$cos_zenith > 0, ]
  if (nrow(df) == 0) return(empty_plot("The sun never rises on this date"))
  p <- ggplot(df, aes(x = clocktime)) +
    geom_ribbon(aes(ymin = 0, ymax = clearsky), fill = PAL[["main"]], alpha = 0.12) +
    geom_line(aes(y = G0, colour = "Extraterrestrial (horizontal)"), linewidth = 0.7,
              linetype = "dashed") +
    geom_line(aes(y = clearsky, colour = "Clear sky (Hottel)"), linewidth = 0.85)
  vals <- c(`Extraterrestrial (horizontal)` = "#9aa7b4",
            `Clear sky (Hottel)` = PAL[["main"]])
  if (tilted) {
    p <- p + geom_line(aes(y = beam_tilted, colour = "On the tilted collector"),
                       linewidth = 0.75)
    vals <- c(vals, `On the tilted collector` = PAL[["accent"]])
  }
  p + scale_colour_manual(values = vals) +
    scale_x_datetime(date_labels = "%H:%M") +
    labs(x = NULL, y = "W/m2") +
    theme_solar()
}

gg_sun_path <- function(paths) {
  ggplot(paths, aes(x = azimuth, y = altitude, colour = label)) +
    geom_hline(yintercept = 0, colour = "#c9d1da", linewidth = 0.4) +
    geom_vline(xintercept = 0, colour = "#c9d1da", linewidth = 0.4) +
    geom_line(linewidth = 0.85) +
    scale_x_continuous(breaks = seq(-180, 180, 45),
                       labels = c("N", "NE", "E", "SE", "S", "SW", "W", "NW", "N")) +
    scale_colour_manual(values = c("#4a7fb5", "#7aa64f", PAL[["accent"]], "#8e5aa8")) +
    labs(x = NULL, y = "Altitude [deg]") +
    theme_solar()
}

gg_analemma <- function(data) {
  key <- data[format(data$date, "%d") == "01", ]
  ggplot(data, aes(x = E, y = declination)) +
    geom_hline(yintercept = 0, colour = "#c9d1da", linewidth = 0.4) +
    geom_vline(xintercept = 0, colour = "#c9d1da", linewidth = 0.4) +
    geom_path(colour = PAL[["main"]], linewidth = 0.8) +
    geom_point(data = key, colour = PAL[["accent"]], size = 1.8) +
    geom_text(data = key, aes(label = format(date, "%b")), size = 3,
              hjust = -0.25, colour = PAL[["accent"]]) +
    scale_x_continuous(expand = expansion(0.12)) +
    labs(x = "Equation of time [min]", y = "Declination [deg]") +
    theme_solar()
}


# --------------------------------------------------------------------------- #
# Fitted seasonal components and monthly mixture parameters
# --------------------------------------------------------------------------- #

#' One fitted seasonal component over the year
gg_seasonal_component <- function(data, variable, meta) {
  df <- data |>
    dplyr::group_by(n) |>
    dplyr::summarise(value = mean(.data[[variable]], na.rm = TRUE), .groups = "drop")
  peak <- df[which.max(df$value), ]
  low  <- df[which.min(df$value), ]

  ggplot(df, aes(x = n, y = value)) +
    geom_line(colour = PAL[["main"]], linewidth = 0.85) +
    geom_point(data = rbind(peak, low), colour = PAL[["accent"]], size = 2.2) +
    geom_text(data = rbind(peak, low), aes(label = sprintf("%.3f", value)),
              vjust = -0.9, size = 3.1, colour = PAL[["accent"]]) +
    scale_x_continuous(breaks = c(1, 60, 121, 182, 244, 305, 366),
                       labels = c("Jan", "Mar", "May", "Jul", "Sep", "Nov", "Dec"),
                       expand = expansion(0.02)) +
    scale_y_continuous(expand = expansion(0.1)) +
    labs(subtitle = sprintf("%s  [%s]", meta$symbol, meta$unit),
         x = "Day of the year", y = NULL) +
    theme_solar()
}

#' One group of monthly mixture parameters
gg_mixture_group <- function(params, group) {
  cols <- names(group$columns)
  df <- do.call(rbind, lapply(cols, function(cl) {
    data.frame(Month = params$Month, value = params[[cl]],
               series = unname(group$columns[[cl]]))
  }))
  df$series <- factor(df$series, levels = unname(group$columns))
  df$MonthLab <- factor(MONTH_ABB[df$Month], levels = MONTH_ABB)

  p <- ggplot(df, aes(x = MonthLab, y = value, group = series, colour = series))
  if (isTRUE(group$zero)) {
    p <- p + geom_hline(yintercept = 0, colour = "#c9d1da", linewidth = 0.4)
  }
  p + geom_line(linewidth = 0.75) +
    geom_point(size = 2.1) +
    scale_colour_manual(values = stats::setNames(
      c(PAL[["dw"]], PAL[["up"]]), unname(group$columns))) +
    scale_y_continuous(expand = expansion(0.12)) +
    labs(x = NULL, y = group$ylab) +
    theme_solar()
}


#' Shade a date range across the full height of a plotly chart
#'
#' Two traps here. `annotate("rect", ymin = -Inf, ymax = Inf)` survives ggplotly
#' as a trace carrying literal -Inf/Inf, which plotly.js cannot draw, so the band
#' silently never appears. And a shape added with `layout()` before the build is
#' thrown away: ggplotly regenerates `layout$shapes` from the theme, panel border
#' included. So build first, then append, in the numeric day units ggplotly gives
#' a Date axis.
plotly_shade_x <- function(p, from, to, colour = PAL[["accent"]], alpha = 0.12,
                           label = NULL) {
  b  <- plotly::plotly_build(p)
  x0 <- as.numeric(as.Date(from))
  x1 <- as.numeric(as.Date(to))

  shape <- list(
    type = "rect", xref = "x", yref = "paper",
    x0 = x0, x1 = x1, y0 = 0, y1 = 1,
    fillcolor = colour, opacity = alpha, layer = "below",
    line = list(width = 0)
  )
  b$x$layout$shapes <- c(b$x$layout$shapes %||% list(), list(shape))

  if (!is.null(label)) {
    ann <- list(
      x = x0, y = 1, xref = "x", yref = "paper",
      xanchor = "left", yanchor = "bottom", yshift = 3,
      text = label, showarrow = FALSE,
      font = list(size = 10, color = colour)
    )
    b$x$layout$annotations <- c(b$x$layout$annotations %||% list(), list(ann))
  }
  b
}
