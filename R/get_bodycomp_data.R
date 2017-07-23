#' Get body comp data from fitbit.com
#'
#' Get body comp data from fitbit using cookie returned from login function
#' @param cookie Cookie returned after login, specifically the "u" cookie
#' @param start_date Date in YYYY-MM-DD format
#' @param end_date Date in YYYY-MM-DD format
#' @keywords data
#' @export
#' @return A dataframe with five columns:
#'  \item{time}{A POSIXct time value}
#'  \item{lean}{The data column corresponding to lean body mass}
#'  \item{weight}{The data column corresponding to weight}
#'  \item{fat}{The data column corresponding to fat body mass}
#'  \item{bf_pct}{The data column corresponding to body fat percent}
#' @examples
#' \dontrun{
#' get_bodycomp_data(cookie, start_date="2015-01-13", end_date="2015-01-20")
#' }
#' get_bodycomp_data

get_bodycomp_data <- function(cookie, start_date,end_date){
  
  if(!is.character(cookie)){stop("cookie must be a character string")}
  if(!is.character(start_date)){stop("start_date must be a character string")}
  if(!is.character(end_date)){stop("end_date must be a character string")}
  if(!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", start_date)){stop('start_date must have format "YYYY-MM-DD"')}
  if(!grepl("[0-9]{4}-[0-9]{2}-[0-9]{2}", end_date)){stop('end_date must have format "YYYY-MM-DD"')}  
  
  url <- "https://www.fitbit.com/graph/getNewGraphData"
  query <- list(type = "bodyComp", dateFrom = start_date, dateTo = end_date)
  response <- httr::GET(url, query = query, httr::config(cookie = cookie))
  dat_string <- methods::as(response, "character")
  dat_list <- jsonlite::fromJSON(dat_string)
  df <- cbind(dat_list[["graph"]][["dataSets"]][["leanMass"]][["dataPoints"]][,1:2],
              dat_list[["graph"]][["dataSets"]][["weight"]][["dataPoints"]][,2]
  )
  names(df)[1:3] <- c("time", "lean","weight")
  tz <- Sys.timezone()
  if (is.null(tz)) {
    tz <- format(Sys.time(), "%Z")
  }
  df$time <- as.POSIXct(df$time, "%Y-%m-%d %H:%M:%S", tz = tz)
  df$fat <- df$weight - df$lean
  df$bf_pct <- df$fat / df$weight
  return(df)
}


