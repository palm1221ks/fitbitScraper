get_bodycomp_data <- function(cookie, start_date,end_date){
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
