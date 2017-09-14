#' Create object containing table data and metadata
#'
#' The object can be used as a dataframe, and the
#' metadata can be accessed with the functions
#'
#' - segment.seg
#' - segment.start_time
#' - segment.filename
#' 
#' @param states a vector of states in {0,1}
#' @param dwells a vector of dwell durations (same length as states)
#' @param seg The segment number. Defaults to 1
#' @param start_time When the dwells began. Defaults to 0
#' @param name Suffix-less version of the original filename. 60uM.dwt -> '60uM'
#' @return The segment object: A dataframe with extra metadata.
#' @examples
#' \dontrun{
#' segment <- segment.create(states, dwells, seg=12, start_time=0, name="60uMc")
#' segment.name(segment)
#' > "60uMc"
#' }
#' @export
segment.create <- function (states, dwells, seg=1, start_time=0, name="burst") {

    data  <- data.frame(states, dwells)

    attr(data, "name") <- name
    attr(data, "seg")  <- seg
    attr(data, "start_time")  <- start_time

    return(data)
    
}


#' Extract segment number from segment
#'
#' @param segment the segment object
#' @return Segment number (integer)
#' @examples
#' \dontrun{
#' segment.seg(data)
#' > 12
#' }
#' @export
segment.seg <- function(segment) {attr(segment, "seg")}


#' Extract start_time from segment
#'
#' @param segment the segment object
#' @return Segment start_time (float)
#' @examples
#' \dontrun{
#' segment.start_time(data)
#' > 17.123295
#' }
#' @export
segment.start_time <- function(segment) {attr(segment, "start_time")}


#' Extract name from segment
#'
#' @param segment the segment object
#' @return Segment name (string)
#' @examples
#' \dontrun{
#' segment.name(data)
#' > "60uMc"
#' }
#' @export
segment.name <- function(segment) {attr(segment, "name")}


#' Get duration of a segment
#'
#' @param segment the segment object
#' @return the duration
#' @examples
#' \dontrun{
#' segment.duration(data)
#' > 2.11
#' }
#' @export
segment.duration <- function(segment) {
    sum(segment$dwells)
}




#' Extract number of open dwells
#'
#' @param segment the segment object
#' @return number of open dwells
#' @examples
#' \dontrun{
#' segment.count_open(data)
#' > 5
#' }
#' @export
segment.count_open<- function(segment) {sum(segment$states == 1)}




#' Extract number of closed dwells
#'
#' @param segment the segment object
#' @return number of closed dwells
#' @examples
#' \dontrun{
#' segment.count_closed(data)
#' > 5
#' }
#' @export
segment.count_closed<- function(segment) {sum(segment$states == 0)}





#' Extract open dwells
#'
#' @param segment the segment object
#' @return the open dwells
#' @examples
#' \dontrun{
#' open_dwells <- segment.open_dwells(data)
#' }
#' @export
segment.open_dwells <- function(segment) { subset(segment, states == 1)$dwells }



#' Extract closed dwells
#'
#' @param segment the segment object
#' @return the closed dwells
#' @examples
#' \dontrun{
#' closed_dwells <- segment.closed_dwells(data)
#' }
#' @export
segment.closed_dwells <- function(segment) { subset(segment, states == 0)$dwells }




#' Calculate empirical P(Open) of a segment
#'
#' NOTE: Assuming that burst starts and ends with 1
#'
#' @param segment The dwells and states table
#' @return The ratio of open time to total time
#' @examples
#' \dontrun{
#' segment.popen(segment)
#' > 0.22
#' }
#' @export
segment.popen <- function (segment) {

    open_times <- subset(segment, states == 1, select=dwells)

    total_duration <- sum(segment$dwells)
    
    return (sum(open_times) / total_duration)
}




#' Calculate empirical P(Closed) of a segment
#'
#' NOTE: Assuming that burst starts and ends with 1
#'
#' @param segment The dwells and states table
#' @return The ratio of closed time to total time
#' @examples
#' \dontrun{
#' segment.pclosed(segment)
#' > 0.78
#' }
#' @export
segment.pclosed <- function (segment) {

    popen <- segment.popen(segment)
    
    return ( 1 - popen )
}
