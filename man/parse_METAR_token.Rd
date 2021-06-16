\name{parse_METAR_token}
\alias{parse_METAR_token}
\title{Decode a METAR token into constituent parts}
\usage{
parse_METAR_token(i)
}
\arguments{
  \item{i}{A string token, found separated by spaces in a METAR string. E.g. a METAR reading of \emph{METAR ZZZZ 010825Z 18010KT CAVOK 05/02 Q1000 NOSIG} will have tokens "\emph{METAR}", "\emph{ZZZZ}", "\emph{010825Z}", and etc.}
}
\description{
Decodes information conveyed by a particular METAR token and returns a named list of relevant data values based on its type classification by \code{identify_METAR_token}.
}
\examples{
parse_METAR_token("18010KT")
parse_METAR_token("NOSIG")
}
