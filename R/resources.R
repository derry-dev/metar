# METAR_wxTokens <- c(
#   "BR", # Mist
#   "DS", # Dust Storm
#   "DU", # Widespread Dust
#   "DZ", # Drizzle
#   "FC", # Funnel Cloud
#   "FG", # Fog
#   "FU", # Smoke
#   "FZ", # Freezing
#   "GR", # Hail
#   "GS", # Small Hail
#   "HZ", # Haze
#   "IC", # Ice Crystals
#   "MI", # Shallow
#   "PL", # Ice Pellets
#   "PO", # Dust Devils
#   "PR", # Partial
#   "RA", # Rain
#   "SA", # Sand
#   "SG", # Snow Grains
#   "SH", # Shower
#   "SN", # Snow
#   "SQ", # Squall
#   "SS", # Sandstorm
#   "TS", # Thunderstorm
#   "UP", # Unidentified Precipitation
#   "VA" # Volcanic Ash
# )

METAR_prefwxTokens <- c(
  "-", # Light
  "\\+", # Heavy
  "BC", # Patches
  "RE", # Recent
  "VC" # Nearby
)

METAR_miscwxTokens <- c(
  "NSW", # No Significant Weather
  "NOSIG" # No Significant Change
)

METAR_dirTokens <- c(
  "N", "NE", "E", "SE", "S", "SW", "W", "NW"
)

METAR_cloudamountTokens <- c(
  "FEW", "SCT", "BKN", "OVC"
)

METAR_cloudconvTokens <- c(
  "CB", "TCU"
)

METAR_cloudmiscTokens <- c(
  "CAVOK", "NSC"
)

METAR_trendTokens <- c(
  "BECMG", "TEMPO"
)

METAR_regex <- list(
  time = "^([0-3]{1}[0-9]{1})([0-2]{1}[0-9]{1})([0-5]{1}[0-9]{1})Z$",
  wind = "^([0-3]{1}[0-9]{2}|VRB)([0-9]{2})(G[0-9]{2}|)(KT|MPS|KPH)$",
  wind_vdir = "^([0-3]{1}[0-9]{2})V([0-3]{1}[0-9]{2})$",
  vsby = paste0("^([0-9]{4})$"),
  dvsby = paste0("^([0-9]{4})(", paste(METAR_dirTokens, collapse = "|"), ")$"),
  rvr = "^R?([0-3]{1}[0-9]{1}[L|C|R]?)/(P|M|)([0-9]{4})(U|D|N|)$",
  cloud = paste0("^(", paste(METAR_cloudamountTokens, collapse = "|"), "|///)([0-9]{3}|///)$"),
  cloud_conv = paste0("^(", paste(METAR_cloudamountTokens, collapse = "|"), "|///)([0-9]{3}|///)(", paste(METAR_cloudconvTokens, collapse = "|"), "|[/]{1,3}|)$"),
  cloud_alt = paste0("^(", paste(METAR_cloudmiscTokens, collapse = "|"), ")$"),
  vv = "^VV([0-9]{3}|///)$",
  tempdew = "(^M?)([0-9]{2})/(M?)([0-9]{2})$",
  qnh = "^Q([0-9]{4})$",
  trend = paste0("^(", paste(METAR_trendTokens, collapse = "|"), ")$"),
  windshear = "^(WS)$",
  metar = "^(METAR)$",
  apt = "^([A-Z]{4})$",
  wx = paste0("^(", paste(METAR_prefwxTokens, collapse = "|"), "|)([A-Z]+)$"),
  unknown = "^(.*)$"
)

METAR_headers <- c(
  "apt",
  "day",
  "hour",
  "min",
  "wind_hdg",
  "wind_vrb",
  "wind_speed",
  "wind_gust",
  "wind_unit",
  "wind_hdg_min",
  "wind_hdg_max",
  "vsby_dist",
  paste0(c("vsby_dir_dist_", "vsby_dir_hdg_"), rep(1:100, each = 2)),
  paste0(c("rvr_rwy_", "rvr_code_", "rvr_vis_", "rvr_trend_"), rep(1:100, each = 4)),
  "wx",
  paste0(c("skyc_", "skyl_", "skyx_"), rep(1:100, each = 3)),
  "vv",
  "tmp",
  "dwp",
  "qnh",
  "trend",
  "ws",
  "unhandled",
  "unknown"
)
METAR_headers <- factor(METAR_headers, levels = METAR_headers)

METAR_duphandle <- list(
  increment = "^cloud(_[a-z]{3,4}|)$|^rvr$|^dvsby$",
  merge = "^wx$|^.*unknown$",
  ignoreextra = "^wind_unit$"
)
