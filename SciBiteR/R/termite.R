#' TERMite
#'
#' \code{TERMite} passes user-specified information to a TERMite API endpoint.
#' Operation of this function requires a valid license to TERMite provided by
#' SciBite.
#'
#' @param endpoint A string specifying the IP and API endpoint of TERMite.
#'   Defaults to "http://localhost:9090/termite" representing a typical
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
#' @param VOCabs A character vector enumerating the VOCabs to be used for
#'   annotation. Defaults to \code{NULL}, all primary VOCabs will be applied.
#' @param options A named list enumerating the runtime options to be used. Names
#'   correspond to valid TERMite options and values correspond to valid option
#'   settings.
#' @param headers A character vector enumerating the HTTP headers to be added to
#'   the HTTP request. Defaults to \code{NULL}, which will not add any extra
#'   HTTP headers to the HTTP request.
#' @example
#' endpoint <- "http://localhost:9090/termite"
#' input <- "macrophage colony stimulating factor"
#' inputFile <- FALSE
#' inputFileFormat <- NULL
#' outputFile <- NULL
#' outputFormat <- "json"
#' VOCabs <- NULL
#' options <- list("subsume" = "false", "rejectAmbig" = "true")
#' TERMite(endpoint="http://localhost:9090/termite",
#'         input = input, inputFile = inputFile, inputFileFormat = inputFileFormat,
#'         outputFile = outputFile,
#'         outputFormat = outputFormat,
#'         VOCabs = VOCabs,
#'         options = options)
#' @export

TERMite <- function(endpoint="http://localhost:9090/termite",
                    input,
                    inputFile = FALSE,
                    inputFileFormat = NULL,
                    outputFile = NULL,
                    outputFormat = "json",
                    VOCabs = NULL,
                    options = NULL,
                    headers = NULL){

  # Error Block -----
  # Input file specificed but no format specified
  if (inputFile != FALSE & is.null(inputFileFormat)) stop("Must specify file format of input file.")

  #  Payload creation -----

  # Initialize List
  payload <- list(
    # Payload Core
    output = outputFormat,
    entities = paste(VOCabs, collapse=','),
    opts = paste(names(options), options, sep='=', collapse='&'),

    # Hidden Options
    rejectMinor = 'on',
    hitFilterWorkspace= '',
    hitFilter='',
    docFilter='',
    fieldTarget='',
    sfunc='',
    wrk='',
    priority='',
    bginfo='',
    username='',
    usertoken='',
    togswapon ='',
    format_off='',
    output_off='',
    bundle='',
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

  # POST -----
  response_raw <- httr::POST(
    url = endpoint,
    body = payload,
    encode = "multipart",
    httr::add_headers(headers)
  )

  #response_json <- fromJSON(response, flatten = FALSE)

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


#' Get entity information
#'
#' Entity lookup function, given an entity type (e.g. GENE, INDICATION) and entity ID (e.g. CSF1, D010024),
#' creates and runs GET call of the format: http://localhost:9090/termite/toolkit/tool.api?t=describe&id=INDICATION:D001249.
#'
#' @param termite_home A string specifying the IP and API endpoint of TERMite.
#'   Defaults to "http://localhost:9090/termite" representing a typical
#'   installation.
#' @param entity_id A string representing an entity ID of interest.
#' @param entity_type A string representing an entity type of interest.
#' @return TERMite JSON.
#' @example
#' getEntity <- get_entity(termite_home="http://localhost:9090/termite",
#'                         entityID="D009103",
#'                          entityType="INDICATION")
#' @export

get_entity <- function(termite_home = "http://localhost:9090/termite", entity_id, entity_type) {
  response_raw <- httr::GET(url = sprintf("%s/toolkit/tool.api?t=describe&id=%s:%s", termite_home, entity_type, entity_id))

  # Error Handing -----
  if (httr::http_error(response_raw)) {
    stop(sprintf ("API request failed [%s]", status_code(response_raw)))
  }

  # Output -----
  response <- httr::content(response_raw, as = "parsed")

  # Error handing -----
  if (response$TOOL_ERROR == TRUE) {
    stop(sprintf("API request error %s", response$TOOL_ERROR_MESSAGE))
  } else {
    return(response)
  }
}


#' Get entity hits from TERMite TSV
#'
#' Converts TERMite TSV response to dataframe of entity hits.
#'
#' @param termite_tsv_response TSV TERMite response object
#' @return dataframe
#' @export

get_entity_hits_from_tsv <- function(termite_tsv_response) {
  rows <- as.list(unlist(strsplit(termite_tsv_response, split = "\n")))

  # remove lines of meta data
  MetaBool <- stringr::str_detect(test[1:20], "#!META::")
  x <- length(MetaBool[MetaBool==T])

  mydf <- read.table(text = termite_tsv_response, sep='\t', header = T, fill = TRUE, comment.char = "", skip = x)
  mydf <- head(mydf,-1)  # remove last comment line

  return(mydf)
}




#' Get entity hits from TERMite JSON
#'
#' Converts TERMite JSON response to dataframe of entity hits.
#'
#' @param termite_json_response JSON TERMite response object
#' @param columns_to_return character list of TERMite JSON attributes to include in the output
#' @return dataframe
#' @export

get_entity_hits_from_json <- function(termite_json_response,
                                      columns_to_return = c("hitID", "entityType", "name", "hitCount", "totnosyns", "goodSynCount",
                                                            "realSynList", "score", "exact_string", "frag_vector_array")) {
  allHitsDF = data.frame()
  
  # Payload processing -----
  if ('RESP_MULTIDOC_PAYLOAD' %in% names(termite_json_response)) {
    # Multidoc processing -----
    columns_to_return <- append(columns_to_return, c("docID"))
    for (document in termite_json_response[['RESP_MULTIDOC_PAYLOAD']]) {
      for (entity_type in document) {
        columnNames <- (names(entity_type[[1]]))
        
        grabInfo <- function(var) {
          # Source: http://zevross.com/blog/2015/02/12/using-r-to-download-and-parse-json-an-example-using-data-from-an-open-data-portal/
          sapply(entity_type, function(x) returnData(x, var))
        }
        returnData <- function(x, var) {
          # get data - entries with multiple values are converted to pipe separated character
          # Source: http://zevross.com/blog/2015/02/12/using-r-to-download-and-parse-json-an-example-using-data-from-an-open-data-portal/
          if(!is.null( x[[var]])){
            if (!is.list(typeof(x[[var]]))) {
              return(stringr::str_c(as.character(x[[var]]), collapse = ' | '))
            }
          }else{
            return(NA)
          }
        }
        # get values and return dataframe
        entityHits <- as.vector(sapply(columnNames, grabInfo), mode = "character")
        entityHitsDF <- as.data.frame(matrix(entityHits, ncol=length(columnNames)), stringsAsFactors=FALSE)
        colnames(entityHitsDF) <- columnNames
        allHitsDF <- plyr::rbind.fill(allHitsDF, entityHitsDF)
      }
    }
  } else {
    # Single payload processing -----
    for (entity_type in termite_json_response[['RESP_PAYLOAD']]) {
      columnNames <- (names(entity_type[[1]]))
      
      grabInfo <- function(var) {
        #print(paste("Variable", var, sep=" "))
        sapply(entity_type, function(x) returnData(x, var))
      }
      
      returnData <- function(x, var) {
        # get data - entries with multiple values are converted to pipe separated character
        # Source: http://zevross.com/blog/2015/02/12/using-r-to-download-and-parse-json-an-example-using-data-from-an-open-data-portal/
        if(!is.null( x[[var]])){
          if (!is.list(typeof(x[[var]]))) {
            return(stringr::str_c(as.character(x[[var]]), collapse = ' | '))
          }
        } else {
          return(NA)
        }
      }
      
      # get values and return dataframe
      entityHits <- as.vector(sapply(columnNames, grabInfo), mode = "character") # extraction of data from all columns and assembly into vector
      entityHitsDF <- as.data.frame(matrix(entityHits, ncol=length(columnNames)), stringsAsFactors=FALSE)
      colnames(entityHitsDF) <- columnNames
      allHitsDF <- plyr::rbind.fill(allHitsDF, entityHitsDF)
    }
  }
  
  # Output -----
  allHitsDF <- transform(allHitsDF, invalidPositions = as.factor(invalidPositions),
                         hitCount = as.numeric(hitCount),
                         totnosyns = as.numeric(totnosyns),
                         goodSynCount = as.numeric(goodSynCount),
                         nonambigsyns = as.numeric(nonambigsyns),
                         score = as.numeric(score),
                         entityType = as.factor(entityType),
                         rejected = as.factor(rejected),
                         dependencyMet = as.factor(dependencyMet),
                         fuzzyMatches = as.numeric(fuzzyMatches))
  returnDF <- allHitsDF[, columns_to_return]
  return(returnDF)
}


#' Get entity hits from TERMite XML
#'
#' Converts TERMite XML response to dataframe of entity hits.
#'
#' @param termite_xml_response XML TERMite response object
#' @return dataframe
#' @export
#'
get_entity_hits_from_xml <- function(termite_xml_response) {
  # Processing -----
  result <- XML::xmlParse(termite_xml_response)
  nodeSet <- XML::getNodeSet(result, "//Hit")
  df = XML::xmlToDataFrame(XML::getNodeSet(result, "//Hit"))

  # Get additional attr -----
  df['Source'] <- sapply(XML::getNodeSet(result, "//Source"), XML::xmlGetAttr, "document_id")
  df$HitType <- sapply(XML::getNodeSet(result, "//Hit"), XML::xmlGetAttr, "type")
  df$HitID <- sapply(XML::getNodeSet(result, "//Hit"), XML::xmlGetAttr, "id")

  return(df)
}
