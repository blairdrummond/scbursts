#' Calculate empirical P(Open) of a *burst*
#'
#' NOTE: Removing first and last state, as they are pauses.
#'
#' @param table The dwells and states table
#' @return The ratio of open time to total time
#' @examples
#' popen(table)
#' > 0.22
#' @export
popen <- function (table) {

    open_times   <- subset(table, states == 1, select=dwells)

    dwells <- table$dwells

    ### NOTE: Removing first and last state, as they are pauses.
    total_duration <- sum(dwells[2:(length(dwells)-1)])

    return (sum(open_times) / total_duration)
}
