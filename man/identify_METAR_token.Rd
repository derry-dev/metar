\name{identify_METAR_token}
\alias{identify_METAR_token}
\title{Identify type of a METAR token}
\usage{
identify_METAR_token(i)
}
\arguments{
  \item{i}{A string token, found separated by spaces in a METAR string. E.g. a METAR reading of \code{METAR ZZZZ 010825Z 18010KT CAVOK 05/02 Q1000 NOSIG} will have tokens "\code{METAR}", "\code{ZZZZ}", "\code{010825Z}", and etc.}
}
\description{
Determines the type of information conveyed by a particular METAR token.
}
\details{
The METAR token \code{i} is distinguished via regular expression, and returns one of the following:
  \describe{
    \item{\code{metar}}{Token \code{METAR} which usually occur at the start of a reading.}
    \item{\code{apt}}{Airport ICAO code.}
    \item{\code{time}}{Time information containing day, hour and minutes in Zulu/GMT.}
    \item{\code{wind}}{Wind information containing heading (degrees or \code{VRB}), wind speed, optional gusting wind speed and wind speed units (either knots \code{KT}, metres per second \code{MPS} or kilometres per hour \code{KPH}).}
    \item{\code{wind_vdir}}{Wind direction information containing two headings indicating range of varying wind direction.}
    \item{\code{vsby}}{Prevailing visibility in metres.}
    \item{\code{dvsby}}{Prevailing visibility in metres with directional indicator.}
    \item{\code{rvr}}{Runway visual range information containing runway, visibility range in metres and trend visual range change (increasing \code{U}, decreasing \code{D} or no change \code{N}).}
    \item{\code{wx}}{Weather code.}
    \item{\code{cloud}}{Cloud information containing amount (few \code{FEW}, scattered \code{SCT}, broken \code{BKN} or overcast \code{OVC}) and height of cloud base in hundreds of feet.}
    \item{\code{cloud_conv}}{Cloud information containing amount (few \code{FEW}, scattered \code{SCT}, broken \code{BKN} or overcast \code{OVC}), height of cloud base in hundreds of feet, and significant convective cloud (Cumulonimbus \code{CB} or Towering Cumulus \code{TCU}).}
    \item{\code{cloud_alt}}{Alternative cloud information such as "ceiling and visibility OK" \code{CAVOK} or no sigificant cloud \code{NSC}.}
    \item{\code{vv}}{Vertical visibility in hundreds of feet, this is found after a cloud token when sky is obscured.}
    \item{\code{tempdew}}{Air temperature and dew point in whole degrees Celsius.}
    \item{\code{qnh}}{QNH in millibar/hectopascal.}
    \item{\code{trend}}{Trend forecasts starting token, this is either becoming \code{BECMG} or temporary \code{TEMPO}. Note that time group tokens preceding the trend starting token cannot be recognised by \code{identify_METAR_token}}
    \item{\code{windshear}}{Wind shear starting token \code{WS}. Note that wind shear is currently unsupported.}
    \item{\code{unknown}}{When \code{identify_METAR_token} fails to recognise token argument.}
  }
}
\examples{
identify_METAR_token("18010KT")
identify_METAR_token("NOSIG")
}
