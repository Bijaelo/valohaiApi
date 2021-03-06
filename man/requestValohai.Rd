% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/valohai-api.R
\name{requestValohai}
\alias{requestValohai}
\title{General request interface for the valohai API}
\usage{
requestValohai(token, uri, body, query, id, encoding, func, ...)
}
\arguments{
\item{token}{user token}

\item{uri}{the uri to the specific call, see description and examples}

\item{body}{data to be send with the body.}

\item{query}{named list of data to be send with the call}

\item{id}{execution, project or otherwise id}

\item{encoding}{the encoding format for the request. Defaults to json (application/json) but may also be multiform, form or raw. See help(httr::POST) for more information.}

\item{func}{request method to use, usually one of httr::GET, httr::POST, httr::PUT, httr::PATCH or httr::DELETE}

\item{...}{additional parameters passed to func}
}
\value{
a list or tibble containing the request result
}
\description{
General request interface for the valohai API
}
\examples{
\dontrun{
token <- 'YOUR TOKEN HERE'
requestValohai(token = token, uri = 'projects', query = list(limit = 10), func = httr::GET)
}

}
