#' Read a .evt file to a table
#'
#' @param points The data to plot
#' @param title The title of the plot. Defaults to blank 
#' @examples
#' 
#' open_times   <- subset(table, states == 1)$dwells
#' closed_times <- subset(table, states == 0)$dwells
#' 
#' uic.plot_hist(open_times, "Open Times")
#' uic.plot_hist(closed_times, "Closed Times")
#'
#' @export
uic.plot_hist <- function (points,title="") {

    ### x - log
    ### y - sqrt

    main=paste(title, "histogram (sqrt frequency)", sep=" ")
    xlab=paste(title, "(log10)", sep=" ")
    ylab="Frequency (sqrt)"

    tabulate <- hist(log10(points), plot = FALSE)

    breaks <- tabulate$breaks
    breaks <- breaks[-length(breaks)] # remove last break (fencepost thing)

    counts <- tabulate$counts

    plot(breaks, sqrt(counts), type='s', xlab=xlab, ylab=ylab, main=main)

}





