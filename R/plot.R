#' Plot points as a histogram after log transforming 
#'
#' @param points The data to plot
#' @param title The title of the plot. Defaults to blank 
#' @examples
#' 
#' open_times   <- subset(table, states == 1)$dwells
#' closed_times <- subset(table, states == 0)$dwells
#' 
#' cplot.log_root_hist(open_times, "Open Times")
#' cplot.log_root_hist(closed_times, "Closed Times")
#'
#' @export
cplot.log_root_hist <- function (points,title="") {

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












#' Plot Histogram of P(Open)
#'
#' @param chunks List of multiple segments
#' @param title The title of the plot. 
#' @param y_transform Transform the y_axis (for instance, log10 it)
#' @examples
#' 
#' cplot.popen_hist(chunks, "P(Open) Histogram, 2017-09-14")
#' cplot.popen_hist(chunks, "P(Open) Histogram, 2017-09-14", y_transform=log10)
#'
#' @export
cplot.popen_hist <- function (chunks, title="P(Open) Histogram", y_transform=identity) {

    popens <- sapply(chunks, segment.popen)


    main=title
    xlab="P(Open)"
    ylab="Frequency"

    tabulate <- hist(popens, plot = FALSE)

    breaks <- tabulate$breaks
    breaks <- breaks[-length(breaks)] # remove last break (fencepost thing)

    counts <- tabulate$counts

    plot(breaks, y_transform(counts), type='s', xlab=xlab, ylab=ylab, main=main, xlim=c(0,1))

}




#' Plot Histogram of P(Closed)
#'
#' @param chunks List of multiple segments
#' @param title The title of the plot. 
#' @param y_transform Transform the y_axis (for instance, log10 it)
#' @examples
#' 
#' cplot.pclosed_hist(chunks, "P(Closed) Histogram, 2017-09-14")
#' cplot.pclosed_hist(chunks, "P(Closed) Histogram, 2017-09-14", y_transform=log10)
#'
#' @export
cplot.pclosed_hist <- function (chunks, title="P(Closed) Histogram", y_transform=identity) {

    pcloseds <- sapply(chunks, segment.pclosed)

    main=title
    xlab="P(Closed)"
    ylab="Frequency"
    

    tabulate <- hist(pcloseds, plot = FALSE)

    breaks <- tabulate$breaks
    breaks <- breaks[-length(breaks)] # remove last break (fencepost thing)

    counts <- tabulate$counts

    plot(breaks, y_transform(counts), type='s', xlab=xlab, ylab=ylab, main=main, xlim=c(0,1))

}





#' Plot Time Series of P(Open)
#'
#' @param chunks List of multiple segments
#' @param title The title of the plot. 
#' @examples
#' 
#' cplot.popen_ts(chunks, "P(Open) Time Series, 2017-09-14")
#'
#' @export
cplot.popen_ts <- function(chunks, title="P(Open) Time Series", xlim=NULL) {

    XLIM <- xlim
    
    times  <- sapply(chunks, segment.start_time)
    popens <- sapply(chunks, segment.popen)

    plot(times,popens, main=title, ylab="P(Open)", xlab="time", ylim=c(0,1), xlim = XLIM)
    lines(times, popens)

}




#' Plot Time Series of P(Closed)
#'
#' @param chunks List of multiple segments
#' @param title The title of the plot. 
#' @examples
#' 
#' cplot.pclosed_ts(chunks, "P(Closed) Time Series, 2017-09-14")
#'
#' @export
cplot.pclosed_ts <- function(chunks,title="P(Closed) Time Series", xlim=NULL) {

    XLIM <- xlim

    times  <- sapply(chunks, segment.start_time)
    pcloseds <- sapply(chunks, segment.pclosed)

    plot(times,pcloseds, main=title, ylab="P(Closed)", xlab="time", ylim=c(0,1), xlim=XLIM)
    lines(times, pcloseds)
    
}


