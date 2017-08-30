#' Create object containing table data and metadata
#'
#' The object can be used as a dataframe, and the
#' metadata can be accessed with the functions
#'
#' - record.rec
#' - record.start_time
#' - record.filename
#' 
#' @param states a vector of states in {0,1}
#' @param dwells a vector of dwell durations (same length as states)
#' @param rec The record number. Defaults to 1
#' @param start_time When the dwells began. Defaults to 0
#' @param name Suffix-less version of the original filename. 60uM.dwt -> '60uM'
#' @return The record object: A dataframe with extra metadata.
#' @examples
#' \dontrun{
#' record <- record.create(states, dwells, rec=12, start_time=0, name="60uMc")
#' record.name(record)
#' > "60uMc"
#' }
#' @export
record.create <- function (states, dwells, rec=1, start_time=0, name="burst") {

    data  <- data.frame(states, dwells)

    attr(data, "name") <- name
    attr(data, "rec")  <- rec
    attr(data, "start_time")  <- start_time

    return(data)
    
}


#' Extract record number from record
#'
#' @param record the record object
#' @return Record number (integer)
#' @examples
#' \dontrun{
#' record.rec(data)
#' > 12
#' }
#' @export
record.rec <- function(record) {attr(record, "rec")}


#' Extract start_time from record
#'
#' @param record the record object
#' @return Record start_time (float)
#' @examples
#' \dontrun{
#' record.start_time(data)
#' > 17.123295
#' }
#' @export
record.start_time <- function(record) {attr(record, "start_time")}


#' Extract name from record
#'
#' @param record the record object
#' @return Record name (string)
#' @examples
#' \dontrun{
#' record.name(data)
#' > "60uMc"
#' }
#' @export
record.name <- function(record) {attr(record, "name")}


#' Get duration of a record
#'
#' @param record the record object
#' @return the duration
#' @examples
#' \dontrun{
#' record.duration(data)
#' > 2.11
#' }
#' @export
record.duration <- function(record) {
    sum(record$dwells)
}




#' Extract number of open dwells
#'
#' @param record the record object
#' @return number of open dwells
#' @examples
#' \dontrun{
#' record.count_open(data)
#' > 5
#' }
#' @export
record.count_open<- function(record) {sum(record$states == 1)}




#' Extract number of closed dwells
#'
#' @param record the record object
#' @return number of closed dwells
#' @examples
#' \dontrun{
#' record.count_closed(data)
#' > 5
#' }
#' @export
record.count_closed<- function(record) {sum(record$states == 0)}






#' Calculate empirical P(Open) of a record
#'
#' NOTE: Assuming that burst starts and ends with 1
#'
#' @param record The dwells and states table
#' @return The ratio of open time to total time
#' @examples
#' \dontrun{
#' record.popen(record)
#' > 0.22
#' }
#' @export
record.popen <- function (record) {

    open_times <- subset(record, states == 1, select=dwells)

    total_duration <- sum(record$dwells)
    
    return (sum(open_times) / total_duration)
}




#' Calculate empirical P(Closed) of a record
#'
#' NOTE: Assuming that burst starts and ends with 1
#'
#' @param record The dwells and states table
#' @return The ratio of closed time to total time
#' @examples
#' \dontrun{
#' record.pclosed(record)
#' > 0.78
#' }
#' @export
record.pclosed <- function (record) {

    popen <- record.popen(record)
    
    return ( 1 - popen )
}
