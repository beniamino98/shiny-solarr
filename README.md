# solarModel

A Shiny front-end for [`solarr`](https://github.com/beniamino98/solarr): specify and estimate a `solarModel`
on CAMS radiation data, inspect the residuals produced at each stage of the
fitting pipeline, and run the diagnostic tests shipped with the package.

## Running

From the parent directory:

```r
shiny::runApp("shiny/shiny-solarr")
```

or from inside this folder:

```r
shiny::runApp()
```

## What it does

**Data** - the raw series for any of the 18 locations in `solarr::CAMS_data`:
observed GHI against CAMS clear sky, the seasonal profile across years, the
monthly distribution, and the underlying table.

**Solar geometry** - the deterministic quantities behind the model, from
`solarr::seasonalSolarFunctions`. Needs no fitted model. Declination, equation
of time, extraterrestrial irradiance and its daily total, sunset hour angle,
day length and noon altitude, one per tab and all together; the intraday sun
position and Hottel clear-sky irradiance for a chosen date, with collector tilt
and azimuth; the sun path on the solstices and equinoxes; the analemma; and the
day-by-day values as a table and a CSV. The site follows the location picked in
the sidebar, or coordinates typed in by hand.

The *Sunrise / sunset defined as* control chooses the altitude of the sun's
centre at which the event is declared, from `$horizons`: `geometric` (the
centre on the astronomical horizon), `refraction` (-34'), `upper_limb` (-50',
what almanacs publish) and the three twilights. It drives sunrise, sunset, day
length and the sunset hour angle. It deliberately does **not** drive `H_on`:
above the atmosphere there is no refraction, so the extraterrestrial radiation
stays geometric and keeps matching the `H0` the clear-sky model is fitted on -
which is also why `solar_seasonal()` calls `Hon(dates, lat)` without the
altitude, exactly as `seasonalClearsky` does.

Times will not match an almanac to the minute even on `upper_limb`, because the
app passes the site altitude and the resulting horizon dip lengthens the day a
little further: Bologna on 21 June comes out 05:27-21:04 against timeanddate's
05:31-21:00. Published tables normally ignore the observer's elevation.

The intraday frame comes straight from `seasonalSolarFunctions$solar_angles()`,
with `$azimut_angle()` for the signed azimuth and `$clearsky()` for the Hottel
curve. Sunrise, sunset and solar noon are read off the sunset hour angle rather
than scanned from the time grid, so they do not depend on the chosen step, and
they carry daylight saving because `solar_time()` applies it.

`solar_event_time()` in `R/utils_solar.R` is what converts an hour angle into a
clock time. It anchors on the row nearest solar noon and steps 15 degrees per
hour in absolute time, rather than interpolating the grid: the hour angle wraps
from +180 to -180 during the night, and on the two daylight-saving switch-over
days the clock grid repeats or skips an hour, so interpolation is fragile there.

**Estimation** - fires `solarModel$new(spec)$fit()` with the specification set in
the sidebar. Reports the point estimates next to the standard errors from
`spec$std.errors`, the log-likelihood, AIC and BIC from `solarModel_AIC_BIC()`,
the four fitted seasonal components (clear sky, mean radiation, seasonal mean,
seasonal volatility - one per tab), the monthly mixture parameters split into
means, standard deviations and probabilities, and the twelve fitted densities.

**Residuals** - one series at a time along the estimation chain:

| Series | Produced by | Serial correlation expected? |
|---|---|---|
| `Yt_tilde` | `Yt` minus its seasonal mean | yes, this is the ARMA input |
| `eps` | ARMA residuals | no, but the squares stay correlated |
| `eps_tilde` | `eps / sigma_bar` | no, but the squares stay correlated |
| `u_tilde` | `eps_tilde / sigma` (GARCH) | no, and neither in the squares |
| `z_tilde` | `u_tilde` standardized by its monthly mixture moments | no, and neither in the squares |

`z_tilde` is not stored on the model: the app rebuilds it exactly as
`solarModel_test_autocorr()` does. Each series gets the time plot, ACF, ACF of
the squares, PACF, monthly boxplots, a histogram against a normal or against
the fitted mixture, and a QQ-plot. For the mixture the QQ-plot goes through the
PIT, so pooling months stays valid.

**Diagnostics** - the tests exported by `solarr`:

- `solarModel_test_autocorr()` for Breusch-Godfrey, Ljung-Box or Box-Pierce at
  several lags, with the outcome compared against the reference behaviour the
  package encodes;
- `solarModel_test_distribution()` for the monthly Kolmogorov-Smirnov test of
  `u_tilde` against the fitted mixture and against a normal;
- `solarModel_PIT()` / `solarModel_PIT_test()` with the histogram and the ECDF
  against a Kolmogorov-Smirnov band;
- `solarModel_test_LPD()` and `solarModel_test_forecast()`;
- `solarModel_AIC_BIC()`.

The four boxes at the top summarise the outcome. The tests read the controls
when *Run diagnostics* is pressed and after each new fit, so changing a control
does not silently re-estimate everything.

**Export** - the fitted R6 object as `.rds` (readable back with `readRDS()`, or
re-loadable from the sidebar), the tables as CSV, and an R snippet that
reproduces the fit outside the app.

## Layout

```
shiny-solarr/
  app.R                  UI skeleton and the top-level server
  www/styles.css         the visual system: cards, tables, controls
  R/
    aaa_setup.R          packages and shared constants (sourced first)
    constants.R          UI choices, series metadata, responsive column recipes
    utils_model.R        build the spec, fit, unpack coefficients and residuals
    utils_plots.R        ggplot2 helpers, reference densities, value boxes
    utils_tables.R       the single DT constructor every table goes through
    utils_solar.R        deterministic solar geometry for a site
    mod_spec.R           sidebar: data selection and full specification
    mod_data.R           Data panel
    mod_solar.R          Solar geometry panel
    mod_estimate.R       Estimation panel
    mod_residuals.R      Residuals panel
    mod_diagnostics.R    Diagnostics panel
    mod_export.R         Export panel
```

Two layout rules are worth knowing before editing the UI. Every `layout_columns()`
takes its widths from `cols_4()`, `cols_2()`, `cols_split()` or `cols_row()` in
`R/constants.R`: those pass a vector of the same length at every breakpoint,
because mixing a scalar with a vector makes bslib size the grid from the scalar.
And a `layout_columns()` wrapping a single child sets `--bs-columns: 1`, which
cascades into every grid nested inside it - put the card in the `tagList()`
directly instead.

Shiny sources every file under `R/` before `app.R`, into an environment those
files share. Variables created in `app.R` are not visible from `R/`, which is
why the packages and the shared constants live in `R/aaa_setup.R` rather than
in a `global.R`.

## Dependencies

`shiny`, `bslib`, `ggplot2`, `dplyr`, `tidyr`, `DT`, `plotly`, `shinyWidgets`,
`solarr`, and `lmtest` for the Breusch-Godfrey test (the app falls back to a
message if it is missing).

## Publishing to shinyapps.io

`renv.lock` records the package versions used by this app, including the exact
GitHub revision of `solarr`. The deployment uses the official repository at
<https://github.com/beniamino98/solarr>; it does not require the local package
checkout. Keep `renv.lock` selected in the Publish dialog. `.rscignore` excludes
local editor configuration and documentation from the deployment bundle.

Before publishing, restart R so it loads the GitHub-installed package, then
check the deployment dependencies from this folder:

```r
deps <- rsconnect::appDependencies(appDir = ".")
deps[deps$Package == "solarr", ]
```

The `Source` column for `solarr` must be `github`. You can then publish using
the IDE's Publish button. No installation runs inside the app on the server.

On another machine, install the same `solarr` revision before publishing:

```r
lock <- jsonlite::read_json("renv.lock")
remotes::install_github(
  "beniamino98/solarr",
  ref = lock$Packages$solarr$RemoteSha,
  upgrade = "never",
  force = TRUE,
  build_vignettes = FALSE
)
```

After deliberately updating dependencies, regenerate `renv.lock` with
`renv::snapshot(project = ".", prompt = FALSE)` and repeat the dependency check.

## Known limitations

- **`spec$std.errors$params` is a placeholder.** It repeats the values of
  `alpha` and `beta` instead of their standard errors, which would give a
  t-statistic of exactly 1. The transformation bounds are sample extrema of
  `Xt` rather than maximum likelihood estimates, so the app blanks the standard
  error, the statistic and the p-value for that component.

- **`solarModel_test_forecast()` is slow**: it inverts the predictive CDF one
  day at a time, roughly ten seconds per thousand observations, and blocks the
  session while it runs. It sits behind its own button with the expected cost
  printed next to it.
