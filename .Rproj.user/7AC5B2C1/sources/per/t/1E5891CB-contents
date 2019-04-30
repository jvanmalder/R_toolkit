#' TExpress
#'
#' \code{TExpress} passes user-specified information to a TExpress API endpoint.
#' Operation of this function requires a valid license to TExpress provided by
#' SciBite.
#'
#' @param endpoint A string specifying the IP and API endpoint of TERMite.
#'   Defaults to "http://localhost:9090/texpress" representing a typical
#'   installation.
#' @param input A string representing either text to be processed with TERMite
#'   or an absolute filepath to the file to be processed.
#' @param inputFile A boolean, with TRUE indicating that \code{input} should be
#'   considered a filepath. Defaults to \code{FALSE}.
#' @param inputFileFormat A string representing the format of the file to be
#'   processed. Requires that \code{file} be \code{TRUE}. Defaults to
#'   \code{NULL}
#' @param outputFile A string representing the absolute filepath where output
#'   will be written. Defaults to \code{NULL}, output will be written to
#'   terminal.
#' @param outputFormat A string representing the desired format of output.
#'   Defaults to "json".
#' @param pattern A string representing a valid TExpress pattern. If
#'   \code{NULL}, user must supply a bundle to be used.
#' @param bundle A string representing the bundle to be used, Please ensure that
#'   this bunndle is loaded on the server which you are calling.
#'   Defaults to \code{NULL}, no bundle will be used.
#' @param options A named list enumerating the runtime options to be used. Names
#'   correspond to valid TERMite options and values correspond to valid option
#'   settings.
#' @examples
#' # Pattern Example
#' texpressResponse <- TExpress(endpoint = "http://localhost:9090/texpress",
#'                              input = "macrophage colony stimulating factor heart attack elbow",
#'                              inputFile = FALSE,
#'                              inputFileFormat = NULL,
#'                              outputFile = NULL,
#'                              outputFormat = "json",
#'                              pattern = ":(GENE)",
#'                              options = list("subsume" = "false", "rejectAmbig" = "true"))
#' @export

TExpress <- function(endpoint="http://localhost:9090/texpress",
                    input,
                    inputFile = FALSE,
                    inputFileFormat = NULL,
                    outputFile = NULL,
                    outputFormat = "json",
                    pattern,
                    bundle = NULL,
                    options = NULL){
  # Error Block -----

  # Input file specificed but no format specified
  if (inputFile != FALSE & is.null(inputFileFormat)) stop("Must specify file format of input file.")

  # Payload creation -----

  # Initialize List

  payload <- list(
    # Payload Core
    output = outputFormat,
    opts = paste(names(options), options, sep='=', collapse='&'),

    # Hidden Options
    docFilter='',
    maxDocs='',
    tabStyle='')

  # Input control

  if (inputFile) {
    payload[['binary']] <- httr::upload_file(input)
    payload[['format']] <- inputFileFormat
  }
  else {
    payload[["text"]] <- input
    payload[["format"]] <- "txt"
  }

  # Pattern control

  if (!is.null(pattern)) {
    payload[['pattern']] <- pattern
    payload[['bundle']] <- ''
  }
  else {
    payload[['pattern']] <- ''
    payload[['bundle']] <- bundle
  }

  # POST -----

  response_raw <- httr::POST(
    url = endpoint,
    body = payload,
    encode = "multipart"
  )

  # Output -----

  if (!is.null(outputFile)) {
    response <- httr::content(response_raw, as = "text")
    write(response, file=outputFile)
  }
  else {
    response <- httr::content(response_raw, as = "parsed")
    return(response)
  }
}

#' Get entity hits from TExpress JSON
#'
#' Converts TExpress JSON response to dataframe of pattern hits.
#'
#' @param texpress_json_response JSON TERMite response object
#' @return dataframe
#' @export

texpress_hits_from_json <- function(texpress_json_response) {
  allHitsDF = data.frame()
  
  # Error Handling ----
  if (('RESP_TEXPRESS' %in% names(texpress_json_response)) == FALSE) stop("No TExpress hits in JSON response")
  
  # Payload processing -----
  if ('RESP_TEXPRESS' %in% names(texpress_json_response)) {
    for (x in 1:(length(texpress_json_response[['RESP_TEXPRESS']]))) {
      texpressHits <- texpress_json_response[['RESP_TEXPRESS']][x]
      docid <- names(texpressHits)
      for (document in texpressHits) {
        hits <- document
        for (patterns in 1:length(names(hits))) {
          patternHits <- hits[patterns]
          patternID <- names(hits[patterns])
          for (pattern in patternHits) {
            # Skip patterns with no hits -----
            if (length(pattern) == 0) {
              next
            } else {
              # Patterns with hits -----
              for (y in 1:(length(pattern))) {
                for (texmatch in pattern[[y]]['matches']) {
                  for (z in 1:length(texmatch)) {
                    texmatch[[z]][['matchEntities']] <- (stringr::str_c(as.character(texmatch[[z]][['matchEntities']]), collapse = ' | '))
                    hitdf <- as.data.frame(do.call("cbind", texmatch[[z]]), stringsAsFactors=FALSE)
                    hitdf$DocID <- as.factor(docid)
                    hitdf$patternID <- as.factor(patternID)
                    allHitsDF <- plyr::rbind.fill(allHitsDF, hitdf)
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  # Output -----
  allHitsDF <- transform(allHitsDF, start = as.numeric(start),
                         end = as.numeric(end),
                         forward = as.factor(forward),
                         vdirection = as.factor(vdirection),
                         negative = as.factor(negative),
                         conf = as.numeric(conf),
                         sentence = as.numeric(sentence),
                         subsumed = as.factor(subsumed),
                         patternName = as.factor(patternName),
                         originalFragmentStart = as.numeric(originalFragmentStart),
                         originalFragmentEnd = as.numeric(originalFragmentEnd))
  return(allHitsDF)
}
