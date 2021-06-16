\name{parse_METAR}
\alias{parse_METAR}
\title{Decode and tabulate METAR data}
\usage{
parse_METAR(metar, out_format = "data.frame")
}
\arguments{
  \item{metar}{A single METAR string or a vector containing METAR strings.}
  \item{out_format}{One of the following: \emph{list} returns a list with data from each METAR string as a separate list element, \emph{data.frame} returns the data in a data.frame with one row per METAR reading, \emph{data.table} (requires data.table package) returns the data in a data.table with one row per METAR reading.}
}
\description{
Decodes and returns a table of processed METAR data allowing for easier analysis.
}
\details{
The output data will contain the following columns or named sublist elements:
  \describe{
    \item{\code{apt}}{Airport ICAO code (string).}
    \item{\code{day}}{Day of METAR reading (numeric).}
    \item{\code{hour}}{Hour of METAR reading (numeric).}
    \item{\code{min}}{Minute of METAR reading (numeric).}
    \item{\code{wind_hdg}}{Wind heading in degrees (numeric).}
    \item{\code{wind_vrb}}{Boolean indicating variable wind heading (boolean).}
    \item{\code{wind_speed}}{Wind speed (numeric).}
    \item{\code{wind_gust}}{Gusting wind speed (numeric).}
    \item{\code{wind_unit}}{Unit of wind/gust speed (string).}
    \item{\code{wind_hdg_min}}{Minimum value of variable wind heading range (numeric), available when \code{wind_vrb} is \code{TRUE}}
    \item{\code{wind_hdg_max}}{Maximum value of variable wind heading range (numeric), available when \code{wind_vrb} is \code{TRUE}}
    \item{\code{vsby_dist}}{Distance of prevailing visibility (numeric).}
    \item{\code{vsby_dir_dist_*}}{Distance of corresponding prevailing visibility in a certain direction (numeric).}
    \item{\code{vsby_dir_hdg_*}}{Direction of corresponding prevailing visibility in a certain direction (string).}
    \item{\code{rvr_rwy_*}}{Runway of corresponding runway visual range (string).}
    \item{\code{rvr_code_*}}{Code indicating whether RVR is greater than measurable maximum value (\code{P}) or is less than measurable minimum value (\code{M}) of corresponding runway visual range (string).}
    \item{\code{rvr_vis_*}}{Visibility distance of corresponding runway visual range (string).}
    \item{\code{rvr_trend_*}}{Trend of visibility distance change of corresponding runway visual range (string), this is either increasing \code{U}, decreasing \code{D} or no change \code{N}.}
    \item{\code{wx}}{All weather codes (string).}
    \item{\code{skyc_*}}{Cloud amount of corresponding cloud layer (string), this is either few \code{FEW}, scattered \code{SCT}, broken \code{BKN}, overcast \code{OVC} or code indicating "ceiling and visibility OK" \code{CAVOK} or no sigificant cloud \code{NSC}.}
    \item{\code{skyl_*}}{Cloud height in feet of corresponding cloud layer (numeric).}
    \item{\code{skyx_*}}{Extra code indicating significant convective cloud of corresponding cloud layer (string), this is either cumulonimbus \code{CB} or towering cumulus \code{TCU}.}
    \item{\code{vv}}{Vertical visibility in feet (numeric) when sky is obscured.}
    \item{\code{tmp}}{Air temperature in degrees Celsius (numeric).}
    \item{\code{dwp}}{Dew point in degrees Celsius (numeric).}
    \item{\code{qnh}}{QNH in millibar/hectopascal (numeric).}
    \item{\code{trend}}{Trend forecast (string).}
    \item{\code{ws}}{Currently unsupported.}
    \item{\code{unhandled}}{Contains tokens for which a type has been identified, but was not decoded for some reason (string).}
    \item{\code{unknown}}{Contains tokens for which a type has not been identified (string).}
  }
}
\examples{
x <- c(
  "METAR ZZZZ 010825Z 18010KT CAVOK 05/02 Q1000 NOSIG",
  "METAR ZZZZ 101455Z 09008KT 050V130 9999 FEW030 09/03 Q1027 NOSIG",
  "METAR ZZZZ 200525Z VRB02KT 9999 R17L/1200D FEW040 15/14 Q1024 NOSIG"
)

parse_METAR(x[1], out_format = "list")
parse_METAR(x) # Returns data.frame by default
parse_METAR(x, out_format = "data.table")
}
