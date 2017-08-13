#' Calculate pulse lengths
#'
#' !!! TIMES becomes DWELLS !!!
#'
#' @param table with columns "states" and "times"
#' @return A table with one less row, where each row
#'
#' states    dwells
#' 0         0.016010
#'
#' represents pulse in state 0 of duration 0.51231, instead
#' of the time at which the state transitioned.
#' @examples
#' table <- relative_time(table) # updated
#' @export
relative_time <- function(table) {

    states <- table$states
    times  <- table$times
    
    # Calculate the durations, the last one gets thrown away
    dwells <- diff(times)
    
    # remove the first pulse, and ignore the trailing end-state
    states <- states[1:length(states)-1]
    
    data  <- data.frame(states, dwells)
    return(data)
}
