#' Add log-root axes to histogram plot
#'
#' @param points The data to plot
#' @examples
#'
#' \dontrun{
#' hist(open_times, axes=FALSE)
#' cplot.log_root_axes(open_times)
#' }
#'
#' @export
cplot.log_root_axes <- function (points) {

    tabulate <- hist(log10(points), plot = FALSE)

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

    label <- function (x) {
        c(c(x),rep(NaN,9))
    }
    
    x_ticks  <- unlist(sapply(l:u, f))
    x_labels <- unlist(sapply(l:u, label))

    axis(side = 1, at = x_ticks, labels=FALSE)
    axis(side = 1, at = l:u, las = TRUE)
    axis(side = 2, at = y_ticks)
    box()

}






#' Plot Time Series (ts) of P(Open)
#'
#' @param bursts List of multiple segments
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

    times  <- sapply(bursts, segment.start_time)
    popens <- sapply(bursts, segment.popen)

    plot(times,popens, main=main, ylab="P(Open)", xlab="time", ylim=c(0,1), xlim = XLIM)
    lines(times, popens)

}




#' Plot Time Series (ts) of P(Closed)
#'
#' @param bursts List of multiple segments
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

    times  <- sapply(bursts, segment.start_time)
    pcloseds <- sapply(bursts, segment.pclosed)

    plot(times,pcloseds, main=main, ylab="P(Closed)", xlab="time", ylim=c(0,1), xlim=XLIM)
    lines(times, pcloseds)

}
