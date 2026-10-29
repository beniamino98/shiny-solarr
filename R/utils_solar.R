# ---------------------------------------------------------------------------
# Deterministic solar geometry, from solarr::seasonalSolarFunctions.
#
# The geometry is the package's: solar_angles() supplies the whole intraday
# frame and azimut_angle() the signed azimuth. Only the horizontal-plane sun
# position and the Hottel clear-sky curve are added on top.
# ---------------------------------------------------------------------------

SOLAR_VARS <- list(
  declination = list(
    label = "Declination",
    symbol = "delta(n)",
    unit = "degrees",
    note = "Angle between the sun at solar noon and the equatorial plane. Runs between -23.45 and +23.45 degrees and drives everything else."
  ),
  E = list(
    label = "Equation of time",
    symbol = "E(n)",
    unit = "minutes",
    note = "Gap between solar time and mean clock time, caused by the eccentricity of the orbit and the tilt of the axis."
  ),
  Gon = list(
    label = "Extraterrestrial irradiance",
    symbol = "G_on(n)",
    unit = "W/m2",
    note = "Solar constant corrected for the earth-sun distance, on a plane normal to the beam."
  ),
  Hon = list(
    label = "Daily extraterrestrial radiation",
    symbol = "H_on(n)",
    unit = "kWh/m2 per day",
    note = "Daily total on a horizontal surface at the top of the atmosphere. The physical ceiling the clear-sky model is calibrated against."
  ),
  omega_s = list(
    label = "Sunset hour angle",
    symbol = "omega_s(n)",
    unit = "degrees",
    note = "Hour angle at sunset for this latitude. Half the angular length of the day."
  ),
  day_length = list(
    label = "Day length",
    symbol = "2 omega_s / 15",
    unit = "hours",
    note = "Hours between sunrise and sunset."
  ),
  noon_altitude = list(
    label = "Solar altitude at noon",
    symbol = "alpha_s(n)",
    unit = "degrees",
    note = "Height of the sun above the horizon at solar noon."
  )
)

SOLAR_VAR_CHOICES <- stats::setNames(
  names(SOLAR_VARS),
  vapply(SOLAR_VARS, function(x) x$label, character(1))
)

CLIME_CHOICES <- c("No Correction", "Summer", "Winter", "Subartic Summer", "Tropical")

#' Sunrise/sunset conventions offered by the package
#'
#' `seasonalSolarFunctions$horizons` is the altitude of the sun's centre at
#' which each event is declared. Read it from the object rather than restating
#' the numbers, so the app cannot drift from the package.
solar_conventions <- function() {
  h <- solarr::seasonalSolarFunctions$new()$horizons
  labels <- c(
    geometric    = "Geometric (centre on horizon)",
    refraction   = "Refraction (-34')",
    upper_limb   = "Upper limb (-50', almanac)",
    civil        = "Civil twilight (-6 deg)",
    nautical     = "Nautical twilight (-12 deg)",
    astronomical = "Astronomical twilight (-18 deg)"
  )
  stats::setNames(names(h), labels[names(h)])
}

CONVENTION_NOTES <- c(
  geometric    = paste("The centre of the sun on the astronomical horizon. The only",
                       "convention consistent with H_on, so this is what the clear-sky",
                       "model is built on."),
  refraction   = paste("The atmosphere bends the light, so the sun is already visible",
                       "when its centre is 34 arcmin below the horizon."),
  upper_limb   = paste("Refraction plus the solar semi-diameter: the upper edge of the",
                       "disc clears the horizon before the centre. This is the sunrise",
                       "and sunset almanacs publish."),
  civil        = "Enough light to read outdoors without artificial lighting.",
  nautical     = "The horizon is still discernible at sea.",
  astronomical = "Full darkness: the sky no longer brightens."
)

#' Coordinates of a CAMS location
place_coords <- function(place) {
  co <- attr(solarr::CAMS_data[[place]], "coords")
  list(lat = co$lat, lon = co$lon, alt = co$alt)
}

#' Day-of-year deterministic quantities for one site
#'
#' @param year Any year; only the day-of-year enters the formulas, the year
#'   simply fixes the calendar the results are plotted against.
solar_seasonal <- function(lat, alt, year = 2021, method = "spencer",
                           convention = "geometric") {
  sf <- solarr::seasonalSolarFunctions$new(method = method)
  dates <- seq.Date(as.Date(sprintf("%d-01-01", year)),
                    as.Date(sprintf("%d-12-31", year)), by = "1 day")
  data.frame(
    date          = dates,
    n             = solarr::number_of_day(dates),
    declination   = sf$declination(dates),
    E             = as.numeric(sf$E(dates)) / 60,                 # seconds -> minutes
    Gon           = sf$Gon(dates),
    # No `alt` here: H_on is the extraterrestrial radiation integral, and the
    # clear-sky model calls Hon(n, lat). Passing the altitude would raise the
    # horizon and give a figure the fitted H0 does not match.
    Hon           = sf$Hon(dates, lat),
    omega_s       = sf$sunset_hour_angle(dates, lat, alt, convention = convention),
    day_length    = as.numeric(sf$sun_hours(dates, lat, alt, convention = convention)) / 3600,
    noon_altitude = sf$solar_altitude(dates, lat)
  )
}

#' Intraday solar position and clear-sky irradiance for one date
#'
#' Built on `seasonalSolarFunctions$solar_angles()`, which returns the clock
#' and solar time grid, the hour angle, the declination, the sunrise/sunset
#' hour angles, the day length and the cosine of the incidence angle on the
#' collector. Clock times honour daylight saving, since `solar_time()` applies
#' the correction whenever the class is built with `legal_hour = TRUE`.
#'
#' @param beta,gamma Tilt and azimuth of the collector, in degrees.
solar_intraday <- function(lat, lon, alt, date, method = "spencer", beta = 0,
                           gamma = 0, lon_st = 15, clime = "No Correction",
                           by = "5 min", tz = "Europe/Rome",
                           convention = "geometric") {
  sf <- solarr::seasonalSolarFunctions$new(method = method)
  day <- as.Date(date)

  # The package supplies the full geometry of the day in one call.
  ang <- sf$solar_angles(day, lat = lat, lon = lon, alt = alt, lon_st = lon_st,
                         beta = beta, gamma = gamma, by = by, tz = tz,
                         convention = convention)
  LST <- ang$solartime

  # Sun position on the horizontal plane: the collector angles must not enter,
  # so this is a second call with beta = gamma = 0 rather than ang$theta.
  cosZ <- pmin(pmax(sf$incidence_angle(LST, lat, alt, beta = 0, gamma = 0), 0), 1)
  theta_z  <- sf$degree(acos(cosZ))
  altitude <- 90 - theta_z

  # Signed by the package (Duffie eq. 1.6.6): negative east of south in the
  # morning, positive west of it in the afternoon.
  azimuth <- sf$azimut_angle(LST, lat, alt)
  azimuth[cosZ <= 0] <- NA_real_

  # Irradiance on the horizontal plane, above the atmosphere and at ground
  # level through the Hottel transmittances.
  G0 <- sf$Gon(day) * cosZ
  clearsky <- rep(0, length(cosZ))
  lit <- cosZ > 1e-4
  if (any(lit)) {
    clearsky[lit] <- sf$clearsky(cosZ = cosZ[lit], G0 = G0[lit],
                                 alt = alt / 1000, clime = clime)
  }

  data.frame(
    clocktime   = ang$clocktime,
    solartime   = as.POSIXct(LST),
    omega       = ang$omega,
    omega_max   = ang$omega_max,
    day_length  = as.numeric(ang$sun_hours) / 3600,
    cos_zenith  = cosZ,
    altitude    = altitude,
    azimuth     = azimuth,
    cos_theta   = ang$theta,
    G0          = G0,
    clearsky    = clearsky,
    beam_tilted = ifelse(cosZ > 1e-4, clearsky * ang$theta / cosZ, 0)
  )
}

#' Headline facts about a site, for the value-box row
solar_site_facts <- function(seasonal, lat, lon, alt) {
  longest  <- seasonal[which.max(seasonal$day_length), ]
  shortest <- seasonal[which.min(seasonal$day_length), ]
  data.frame(
    metric = c("Latitude", "Longitude", "Altitude",
               "Longest day", "Shortest day", "Day length range",
               "Peak declination", "Peak daily extraterrestrial radiation",
               "Solar altitude at noon (range)"),
    value = c(
      sprintf("%.4f deg", lat),
      sprintf("%.4f deg", lon),
      sprintf("%s m", alt),
      sprintf("%s (%.2f h)", format(longest$date, "%d %B"), longest$day_length),
      sprintf("%s (%.2f h)", format(shortest$date, "%d %B"), shortest$day_length),
      sprintf("%.2f h", max(seasonal$day_length) - min(seasonal$day_length)),
      sprintf("%.2f deg on %s", max(seasonal$declination),
              format(seasonal$date[which.max(seasonal$declination)], "%d %B")),
      sprintf("%.2f kWh/m2 on %s", max(seasonal$Hon),
              format(seasonal$date[which.max(seasonal$Hon)], "%d %B")),
      sprintf("%.2f to %.2f deg", min(seasonal$noon_altitude), max(seasonal$noon_altitude))
    ),
    stringsAsFactors = FALSE
  )
}

#' Clock time of a given hour angle, exactly
#'
#' The hour angle advances 15 degrees per hour of solar time, and solar time
#' advances with absolute clock time, so one reference row fixes every event of
#' the day. Interpolating the grid instead would be both step-limited and
#' fragile: omega wraps from +180 to -180 during the night, and on the two
#' daylight-saving switch-over days the clock grid repeats or skips an hour.
#' Anchoring near omega = 0 keeps the reference far from the wrap, and the
#' arithmetic is in absolute time, so the switch-over is carried correctly.
#'
#' @param intraday A frame from `solar_intraday()`.
#' @param omega Hour angle in degrees.
solar_event_time <- function(intraday, omega) {
  i <- which.min(abs(intraday$omega))
  intraday$clocktime[i] + (omega - intraday$omega[i]) / 15 * 3600
}
