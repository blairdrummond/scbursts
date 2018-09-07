#' Read a .hst file to a table (x, y, fit)
#'
#' Times are in seconds
#'
#' @param filename The filename
#' @param extract Extract either "open" or "closed" histogram
#' @param square Reverse a square-root operation on histogram
#' @return A tables with columns "x", "y" and "fit".
#' @examples
#'
#' library(uottawaionchannel)
#' 
#' \dontrun{
#' mil_hst <- hst.read("data/A1W18_dur.hst", extract="open", square=TRUE)
#' }
#'
#' # import some of the data included with the package
#' infile <- system.file("extdata", "example.hst", package = "uottawaionchannel")
#' table <- hst.read(infile)
#'
#' table
#'
#' @export
#' @importFrom utils read.csv
hst.read <- function (filename, extract="open", square=FALSE) {

    extract <- tolower(extract)
    
    if (extract == "open") {         
        offset <- 0
    } else if (extract == "closed") {
        offset <- 3
    } else {
        stop("extract either \"open\" or \"closed\"")
    }

    ### Assumes file has the format (DOS)
    ### Notice the two blank lines and the leading tabs. Also the note DOS line endings

    ####	histograms bin-sqrt(hst/sum(hst))-sqrt(pdf/sum(hst)):
    ####	0.018840	0.013985	0.016598		0.018878	0.013922	0.007139	
    ####	0.023106	0.362802	0.373325		0.022677	0.096458	0.065489	
    ####	0.028338	0.292684	0.335782		0.027241	0.096458	0.071523	
    ####	0.034754	0.259760	0.291282		0.032723	0.113961	0.078059	
    ####
    ####                                       ...
    ###
    ####	4.660111	0.086209	0.102153		2.666868	0.108738	0.129563	
    ####	5.715281	0.052327	0.075907		3.203560	0.074975	0.101162	
    ####	7.009368	0.039556	0.051607		3.848258	0.053922	0.073837	
    ####	8.596470	0.027970	0.031486		4.622698	0.031132	0.049720	
    ####	10.542933	0.024223	0.016837		5.552989	0.027845	0.030405	
    ####
    ####


    # load lines
    FileInput <- readLines(filename) 

    # Notify user of the header of the file
    write(sprintf("%s header:\n\n\t%s\n", filename, FileInput[1]), stderr())
    
    # Ignore the first line
    table <- read.table(filename, skip=1, sep="\t", header=FALSE)
     
    # extract the open v.s. closed columns
    bin  <- table[,1+offset]
    y    <- table[,2+offset]
    yfit <- table[,3+offset]

    # Correct for the square root
    if (square) {
        y <- y*y
        yfit <- yfit*yfit
    }
    
    data <- data.frame(bin,y,yfit)
    attr(data, "name") <- util.basename(filename)
    
    return(data)

}







#' Extract header from hst file
#'
#' @param filename The filename
#' @return A string containing the header
#' @examples
#' \dontrun{
#' header <- hst.extract_header("data/60uM.hst")
#' 
#' hst.write(open_table, closed_table, file="60uMc.hst", header=header)
#' 
#' }
#' @export
hst.extract_header <- function (filename) {
    # Just return the first line
    return(readLines(filename)[1])
}





#' Write bursts to a .hst file
#'
#' @param segments A segment or list of segments to write to filename
#' @param filename The filename
#' @examples
#' \dontrun{
#'
#' hst.write(, file="60uMc-R.hst")
#'
#' }
#' @export
#' @importFrom utils read.csv
hst.write <- function (open_hist, closed_hist, file="", header=NULL) {
    
    tabs <- rep("", length(open_hist$bin))
    
    # Later code assumes that there are multiple segments
    t <- cbind(tabs, open_hist, tabs, closed_hist, tabs)

    if (is.null(header)) {
        header_string <- "	histograms bin-sqrt(hst/sum(hst))-sqrt(pdf/sum(hst)):\r\n"
    } else {
        header_string <- header
    }
    
    write(header_string, file) 
    write.table(t, file, append=TRUE, sep="\t", col.names=FALSE, row.names=FALSE, eol="\r\n", quote = FALSE) 
    write("\r\n\r\n", file)

}
