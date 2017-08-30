#' Plot dwells as a histogram after log transforming 
#'
#' @param dwells The data to plot
#' @param main The title of the plot. Defaults to blank 
#' @param ... Any and options you can pass to plot, except for
#' @examples
#' 
#' \dontrun{
#' open_times   <- subset(table, states == 1)$dwells
#' closed_times <- subset(table, states == 0)$dwells
#' 
#' cplot.log_root_hist(open_times, "Open Times")
#' cplot.log_root_hist(closed_times, "Closed Times")
#' }
#'
#' @export
#' @seealso plot
#' @importFrom graphics plot hist
cplot.log_root_hist <- function (dwells, ...) {

    tabulate <- hist(log10(dwells), plot = FALSE)

    gaps <- tabulate$breaks
    gaps <- gaps[-length(gaps)] # remove last gap (fencepost thing)

    counts <- tabulate$counts

    v <- 0:ceiling(sqrt(max( counts )))
    y_ticks <- v*v

    l <- floor(min(gaps))
    u <- ceiling(max(gaps))

    f <- function(v) {

        range <- log10(1:10)
        return(range + v)

    }

    x_ticks <- unlist(sapply(l:u, f))

    plot(gaps, counts, type='s', axes=FALSE, ... )

    axis(side = 1, at = x_ticks)
    axis(side = 2, at = y_ticks)
    box()

}












#' Plot Histogram of P(Open)
#'
#' @param bursts List of multiple records
#' @param main The title of the plot. 
#' @param y_transform Transform the y_axis (for instance, log10 it)
#' @examples
#' 
#' \dontrun{
#' cplot.popen_hist(bursts, "P(Open) Histogram, 2017-09-14")
#' cplot.popen_hist(bursts, "P(Open) Histogram, 2017-09-14", y_transform=log10)
#' }
#'
#' @export
#' @importFrom graphics plot hist
cplot.popen_hist <- function (bursts, main="P(Open) Histogram", y_transform=identity) {

    popens <- sapply(bursts, record.popen)


    xlab="P(Open)"
    ylab="Frequency"

    tabulate <- hist(popens, plot = FALSE)

    gaps <- tabulate$breaks
    gaps <- gaps[-length(gaps)] # remove last gap (fencepost thing)

    counts <- tabulate$counts

    plot(gaps, y_transform(counts), type='s', xlab=xlab, ylab=ylab, main=main, xlim=c(0,1))

}




#' Plot Histogram of P(Closed)
#'
#' @param bursts List of multiple records
#' @param main The title of the plot. 
#' @param y_transform Transform the y_axis (for instance, log10 it)
#' @examples
#' \dontrun{
#' cplot.pclosed_hist(bursts, "P(Closed) Histogram, 2017-09-14")
#' cplot.pclosed_hist(bursts, "P(Closed) Histogram, 2017-09-14", y_transform=log10)
#' }
#' @export
#' @importFrom graphics plot hist
cplot.pclosed_hist <- function (bursts, main="P(Closed) Histogram", y_transform=identity) {

    pcloseds <- sapply(bursts, record.pclosed)

    xlab="P(Closed)"
    ylab="Frequency"
    

    tabulate <- hist(pcloseds, plot = FALSE)

    gaps <- tabulate$breaks
    gaps <- gaps[-length(gaps)] # remove last gap (fencepost thing)

    counts <- tabulate$counts

    plot(gaps, y_transform(counts), type='s', xlab=xlab, ylab=ylab, main=main, xlim=c(0,1))

}





#' Plot Time Series of P(Open)
#'
#' @param bursts List of multiple records
#' @param main The title of the plot. 
#' @examples
#' 
#' \dontrun{
#' cplot.popen_ts(bursts, "P(Open) Time Series, 2017-09-14")
#' }
#'
#' @export
#' @importFrom graphics plot lines
cplot.popen_ts <- function(bursts, main="P(Open) Time Series", xlim=NULL) {

    XLIM <- xlim
    
    times  <- sapply(bursts, record.start_time)
    popens <- sapply(bursts, record.popen)

    plot(times,popens, main=main, ylab="P(Open)", xlab="time", ylim=c(0,1), xlim = XLIM)
    lines(times, popens)

}




#' Plot Time Series of P(Closed)
#'
#' @param bursts List of multiple records
#' @param main The title of the plot. 
#' @examples
#' 
#' \dontrun{
#' cplot.pclosed_ts(bursts, "P(Closed) Time Series, 2017-09-14")
#' }
#'
#' @export
#' @importFrom graphics plot lines
cplot.pclosed_ts <- function(bursts, main="P(Closed) Time Series", xlim=NULL) {

    XLIM <- xlim

    times  <- sapply(bursts, record.start_time)
    pcloseds <- sapply(bursts, record.pclosed)

    plot(times,pcloseds, main=main, ylab="P(Closed)", xlab="time", ylim=c(0,1), xlim=XLIM)
    lines(times, pcloseds)
    
}


