
library(httr)
.baseURL <- 'https://app.valohai.com/api'
.version <- 'v0'

.onAttach <- function(libname, pkgname){
  tidyAvail <- suppressMessages(suppressWarnings(suppressPackageStartupMessages(requireNamespace('tidyr', quietly = TRUE))))
  options('valohaiApiPrettify' = tidyAvail, 'valohaiApiPrettifyFunc' = ifelse(tidyAvail, function(x){
    res <- try(tidyr::unnest_wider(tidyr::tibble(col = x), c(col)), silent = TRUE)
    if(inherits(res, 'try-error'))
      return(x)
    res
  },  identity))
  if(tidyAvail)
    packageStartupMessage('valohaiApi has detected that the tidyr package is installed in the current R session, and will attempt to automatic change api call results to a tibble.\nThis behaviour can be changed by setting the \'valohaiApiPRettify\' option to FALSE.')
}

#' Generate uri for https requests
#'
#' @param uri url for the valohai api
#' @param id optional id.
#'
#' @description This is a simple wrapper function which will take any valohai api, possibly with an id and generate the necessary uri for creating handles, posting etc.
#' This function is mainly intended for internal use, but can be used in case of multiple requests, to generate a handle that should be used for all requests which may lead to a slight performance boost.
#'
#' @examples
#' generateUri('executions')
#' generateUri('/executions////')
#' generateUri('executions', 3)
#'
#' @export
generateUri <- function(uri = '', id){
  if(!missing(id))
    if(length(id) == 1)
      uri <- c(uri[1], id, uri[-1])
    else
      stop('id must be of length one in the uri call. All API calls with multiple id\'s contain these within the query.')
  # Clean the first url
  if(nchar(gsub('/', '', uri[1])) != nchar(uri[1]))
    uri[1] <- paste0(basename(uri), '/')
  uri <- gsub('/$|^/', '', gsub('/(/*)', '/', paste0(uri, collapse = '/')))
  paste0(.baseURL, '/', .version, '/', uri, '/')
}

#' Function for prettifying the result of any valohai call.
#'
#' @param x Any result from getValohai, postValohai, putValohai, etc.
#'
#' @return Possibly prettified result of x, given by the function in getOption('valohaiApiPrettifyFunc')
#'
valohaiPrettify <- function(x){
  if(isTRUE(getOption("valohaiApiPrettify")))
    getOption("valohaiApiPrettifyFunc")(x)
  else
    x
}

SecureCall <- function(){
  pf <- parent.frame()
  assign('scipen', getOption('scipen'), envir = pf)
  options('scipen' = 999)
  do.call('on.exit', list(substitute(options('scipen' = scipen))), envir = pf)
}

# To be made: Should try to fetch token if not already provided.
# .getToken(){
#
# }

#' Get a list of all uris available
#'
#' @param token user token
#'
#' @description Simple function for importing a list of all available uris.
#'
#' @examples
#' \dontrun{
#' token <- 'YOUR-TOKEN-HERE'
#' getAllUris(token = token)
#' }
#'
#' @export
getAllUris <- function(token){
  sub('.*/(?=[^/]*/$)', '', getValohai(token = token, uri = ''))
}

testEncoding <- function(encoding, uri){
  if(!is.character(encoding) || length(encoding) != 1)
    stop('encoding must be one of "multipart", "form", "json" or "raw"')
  if(any(c('bulk_delete', 'bulk_stop') %in% uri) && encoding != 'form')
    warning('Uri contains bulk_delete or bulk_stop, but the encoding is not set to ')
  else if(encoding != 'json')
    warning(paste0('Encoding is set to ', encoding, ' but the command is not a bulk command. This might return an error or unexpected result.\nTry setting encoding =\'json\' or checking the api documentation to ensure this is the correct encoding.'))
  else if(!encoding %in% c("multipart", "form", "json", "raw"))
    stop('encoding must be one of "multipart", "form", "json" or "raw"')
}

#' Get requests for valohai
#'
#' @param token user token
#' @param uri the uri to the specific call, see description and examples
#' @param query named list of data to be send with the call
#' @param id execution, project or otherwise id
#' @param ... additional arguments passed to httr::GET
#'
#' @return A list or tibble containing the named results.
#'
#' @description This functions provides a basic interface to the valohai GET requests API.
#' The user it expected to provide a secret token, which should be able to read one or more urls. The available urls can be found using getAllUris() (see example).
#' It is important to note that if the 'uri' has multiple parts, it should be provided as a vector (eg 'executions/{id}/' should be given as c('executions', '{id}')), as the uri will be cleaned before being sent to the valohai API.
#'
#' The user is expected to provide a secret token, the uri directory and possible data that should be send as query.
#'
#' @examples
#' \dontrun{
#' token <- 'YOUR-TOKEN-HERE'
#'
#' # Perform calls to some of the available calls:
#' getValohai(token = token, uri = 'projects', query = list(limit = 10))
#' #Same: getValohai(token = token, uri = c('billing', 'credit'), id = 1)
#' getValohai(token = token, uri = c('billing', 1, 'credit'))
#'
#' getValohai(token = token, uri = 'invoices', query = list(limit = 10, offset = 7, ordering = 'id'))
#'
#' # List available uris (same as getAllUris(token = token))
#' sub(".*/(?=[^/]*/$)", "", getValohai(token = token, uri = ""), perl = TRUE)
#' }
#'
#' @importFrom httr GET add_headers http_error content
#'
#' @export
getValohai <- function(token, uri, query, id, ...){
  SecureCall()
  if(!missing(query) && !is.null(names(query))){
    if(!is.list(query)){
      query <- as.list(query)
      warning('query was converted to a list. This may cause numbers to be in scientific notation, which may yield unexpected results.')
    }
    l <- lengths(query)
    if(any(ind <- l > 1)){
      d <- unlist(query[ind])
      names(d) <- rep(names(query)[ind], l[ind])
      query <- c(query[l == 1], d)
    }
    res <- httr::GET(generateUri(uri, id),
                     config = httr::add_headers(Authorization = paste0('Token ', token)),
                     query = query)
  }else
    res <- httr::GET(generateUri(uri, id),
                     config = httr::add_headers(Authorization = paste0('Token ', token)))
  if(httr::http_error(res)){
    errors <- unlist(httr::content(res))
    stop(paste0('\nRequest error:\n', paste0(names(errors),': ', errors, collapse = '\n\t'), collapse = '\n'))
  }
  con <- httr::content(res)
  if('results' %in% names(con))
    valohaiPrettify(con[['results']])
  else
    con
}


#' General request interface for the valohai API
#'
#' @param token user token
#' @param uri the uri to the specific call, see description and examples
#' @param body data to be send with the body.
#' @param query named list of data to be send with the call
#' @param id execution, project or otherwise id
#' @param encoding the encoding format for the request. Defaults to json (application/json) but may also be multiform, form or raw. See help(httr::POST) for more information.
#' @param func request method to use, usually one of httr::GET, httr::POST, httr::PUT, httr::PATCH or httr::DELETE
#' @param ... additional parameters passed to func
#'
#' @return a list or tibble containing the request result
#'
#'
#' @importFrom httr POST http_error content
#'
#' @examples
#' \dontrun{
#' token <- 'YOUR TOKEN HERE'
#' requestValohai(token = token, uri = 'projects', query = list(limit = 10), func = httr::GET)
#' }
#'
#' @export
requestValohai <- function(token, uri, body, query, id, encoding, func, ...){
  if(missing(func))
    stop('function must be a function or character specifying the function to be used for requests.')
  if((is.character(func) && func %in% c('GET', 'httr::GET', 'httr:::GET')) || deparse(substitute(func)) %in% c('GET', 'httr::GET', 'httr:::GET')){
    if(!(mb <- missing(body)) && mq <- missing(query))
      query <- body
    else if (!mb && !mq)
      stop('get requests only have one of either body or query.')
    return(getValohai(token, uri, query, id, ...))
  }
  if(is.character(func))
    func <- get(func, mode = 'function')
  else if(!is.function(func))
    stop('func must be a callable function or a character naming the function to be called.')
  SecureCall()
  if(!missing(body) && !is.null(names(body))){
    if(!is.list(body))
      body <- as.list(body)
  }else
    body <- ""
  testEncoding(encoding, uri)
  res <- func(generateUri(uri, id),
              body = body,
              config = httr::add_headers(Authorization = paste0('Token ', token)),
              encode = encoding,
              ...)
  if(httr::http_error(res)){
    ## I gotta figure some better way to handle the errors from the valohai server. They're often rather long (200++ chars).
    warning('An error was returned from the server. Due to the usual length of these errors the object is returned for further processing.')
    # errors <- unlist(httr::content(res))
    # msg <- paste0('POST error:\n\t', paste0(names(errors),': ', errors, collapse = '\n\t'), collapse = '\n')
    # maxPrint <- getOption("warning.length")
    # options('warning.length' = nchar(msg) + 1000)
    # on.exit(options('warning.length' = maxPrint))
    # stop(msg)
  }
  con <- httr::content(res)
  if('results' %in% names(con))
    valohaiPrettify(con[['results']])
  else
    con
}

#' POST requests for valohai
#'
#' @param token user token
#' @param uri the uri parts
#' @param body data to be send with the body.
#' @param encoding the encoding format for the request. Defaults to json (application/json) but may also be multiform, form or raw. See help(httr::POST) for more information.
#' @param id execution, project or otherwise id
#' @param ... additional arguments passed to httr::POST
#'
#' @return A list or tibble containing the named results.
#'
#' @description This functions provides a basic interface to the valohai POST requests API.
#' The user it expected to provide a secret token, which should be able to read one or more urls. The available urls can be found using getAllUris() (see example).
#' It is important to note that if the 'uri' has multiple parts, it should be provided as a vector (eg 'executions/{id}/' should be given as c('executions', '{id}')), as the uri will be cleaned before being sent to the valohai API.
#'
#'
#' @examples
#'
#' \dontrun{
#' token <- 'YOUR-TOKEN-HERE'
#' postValohai(token,
#'  "executions",
#'  list(command = "command from valohai step"
#'      , commit = "commit id'"
#'      , environment = "environment id"
#'      , image = "rocker/rstudio:latest"
#'      , inherit_environment_variables = TRUE
#'      , inputs = list()
#'      , parameters = list("param name 1" = "hello world")
#'      , step = "download_yahoo"
#'      , tags = list("very useful tag for online gui and subsetting")
#'      , time_limit = 60 * 30 # Time limit.
#'      , project = "project id from getValohai(token, "environment")"
#'      , title = "R execution")
#' )
#' }
#'
#' @export
postValohai <- function(token, uri, body, encoding = 'json', id, ...)
  requestValohai(token = token, uri = uri, body = body, encoding = encoding, id = id, func = httr::POST, ...)


#' PUT requests for valohai
#'
#' @param token user token
#' @param uri the uri parts
#' @param body data to be send with the body.
#' @param encoding the encoding format for the request. Defaults to json (application/json) but may also be multiform, form or raw. See help(httr::POST) for more information.
#' @param id execution, project or otherwise id
#' @param ... additional arguments passed to httr::POST
#'
#' @return A list or tibble containing the named results.
#'
#' @description This functions provides a basic interface to the valohai POST requests API.
#' The user it expected to provide a secret token, which should be able to read one or more urls. The available urls can be found using getAllUris() (see example).
#' It is important to note that if the 'uri' has multiple parts, it should be provided as a vector (eg 'executions/{id}/' should be given as c('executions', '{id}')), as the uri will be cleaned before being sent to the valohai API.
#'
#' @examples
#'
#' \dontrun{
#' token <- 'YOUR-TOKEN-HERE'
#' putValohai(token, "deployment-endpoints", list('enbaled' = FALSE), id = 3)
#' }
#'
#' @export
putValohai <- function(token, uri, body, encoding = 'json', id, ...)
  requestValohai(token = token, uri = uri, body = body, encoding = encoding, id = id, func = httr::PUT, ...)

#' PATCH requests for valohai
#'
#' @param token user token
#' @param uri the uri parts
#' @param body data to be send with the body.
#' @param encoding the encoding format for the request. Defaults to json (application/json) but may also be multiform, form or raw. See help(httr::POST) for more information.
#' @param id execution, project or otherwise id
#' @param ... additional arguments passed to httr::POST
#'
#' @return A list or tibble containing the named results.
#'
#' @description This functions provides a basic interface to the valohai POST requests API.
#' The user it expected to provide a secret token, which should be able to read one or more urls. The available urls can be found using getAllUris() (see example).
#' It is important to note that if the 'uri' has multiple parts, it should be provided as a vector (eg 'executions/{id}/' should be given as c('executions', '{id}')), as the uri will be cleaned before being sent to the valohai API.
#'
#' @examples
#'
#' \dontrun{
#' token <- 'YOUR-TOKEN-HERE'
#' patchValohai(token, "deployment-endpoints", list('enbaled' = FALSE), id = 3)
#' }
#'
#' @export
patchValohai <- function(token, uri, body, encoding = 'json', id, ...)
  requestValohai(token = token, uri = uri, body = body, encoding = encoding, id = id, func = httr::PATCH, ...)

#' delete requests for valohai
#'
#' @param token user token
#' @param uri the uri parts
#' @param body data to be send with the body.
#' @param encoding the encoding format for the request. Defaults to json (application/json) but may also be multiform, form or raw. See help(httr::POST) for more information.
#' @param id execution, project or otherwise id
#' @param ... additional arguments passed to httr::POST
#'
#' @return A list or tibble containing the named results.
#'
#' @description This functions provides a basic interface to the valohai POST requests API.
#' The user it expected to provide a secret token, which should be able to read one or more urls. The available urls can be found using getAllUris() (see example).
#' It is important to note that if the 'uri' has multiple parts, it should be provided as a vector (eg 'executions/{id}/' should be given as c('executions', '{id}')), as the uri will be cleaned before being sent to the valohai API.
#'
#' @examples
#'
#' \dontrun{
#' token <- 'YOUR-TOKEN-HERE'
#' deleteValohai(token, "deployment-endpoints", list('enbaled' = FALSE), id = 3)
#' }
#'
#' @export
deleteValohai <- function(token, uri, body, encoding = 'json', id, ...)
  requestValohai(token = token, uri = uri, body = body, encoding = encoding, id = id, func = httr::DELETE, ...)

