#' Read a .evt file to a table
#'
#' Times are in seconds
#'
#' @param filename The filename
#' @return A list of tables with columns "states" and "times".
#' Each table corresponds to a contiguous segment from a recording.
#' @examples
#'
#' library(uottawaionchannel)
#' 
#' \dontrun{
#' tables <- evt.read("data/60uM.evt")
#' }
#'
#' # import some of the data included with the package
#' infile <- system.file("extdata", "60uM.evt", package = "uottawaionchannel")
#' tables <- evt.read(infile)
#'
#' head(tables[[1]])
#'
#' @export
#' @importFrom utils read.csv
evt.read <- function (filename) {
    
    ### Assumes file has the format

    ### blah blah blah
    ### 
    ### ... 
    ###
    ### Events
    ### 1	0.05027597	6.346019E-012	1.943602E-011	1	0.000000E+000
    ### 1	0.05032199	1.943602E-011	6.346019E-012	0	0.000000E+000
    ### 1	0.05035479	6.346019E-012	1.943602E-011	1	0.000000E+000
    ### 1	0.05090435	1.943602E-011	6.346019E-012	0	0.000000E+000
    ### 1	0.05092179	6.346019E-012	1.943602E-011	1	0.000000E+000

    # load lines
    FileInput <- readLines(filename) 

    # Jump to where the data starts
    skip_line <- tail(grep("^Events$", FileInput), n=1)

    # Read everything past 'Events'
    table <- read.csv(filename, skip=skip_line, sep="\t",header=FALSE)
     
    # extract the needed columns
    all_states <- table[,5]
    all_times  <- table[,2]
    segs <- c(1, which(diff(table[,1]) > 0)+1, length(table[,1])+1)

    segments <- list()
    for (i in 1:(length(segs)-1)) {
        states <- all_states[(segs[i]):(segs[i+1]-1)]
        times <-  all_times[(segs[i]):(segs[i+1]-1)]

        data <- data.frame(states, times)
        attr(data, "name") <- util.basename(filename)
        segments[[i]] <- data
    }
    
    return(segments)

}







#' Extract header from evt file
#'
#' @param filename The filename
#' @return A string containing the header
#' @examples
#' \dontrun{
#' header <- evt.extract_header("data/60uM.evt")
#' 
#' evt.write(segments, header=header, file="60uMc.evt")
#' 
#' }
#' @export
evt.extract_header <- function (filename) {
    
    ### Assumes file has the format

    ### blah blah blah
    ### 
    ### ... 
    ###
    ### Events
    ### 1	0.05027597	6.346019E-012	1.943602E-011	1	0.000000E+000
    ### 1	0.05032199	1.943602E-011	6.346019E-012	0	0.000000E+000
    ### 1	0.05035479	6.346019E-012	1.943602E-011	1	0.000000E+000
    ### 1	0.05090435	1.943602E-011	6.346019E-012	0	0.000000E+000
    ### 1	0.05092179	6.346019E-012	1.943602E-011	1	0.000000E+000

    # load lines
    FileInput <- readLines(filename) 

    # Jump to where the data starts
    header_end <- tail(grep("^Events$", FileInput), n=1)

    header_string <- paste(lines[1:header_end], collapse='\r\n')
    
    return(header_string)

}





#' Write bursts to a .evt file
#'
#' @param segments A segment or list of segments to write to filename
#' @param filename The filename
#' @examples
#' \dontrun{
#'
#' evt.write(segments, file="60uMc-R.evt")
#'
#' }
#' @export
#' @importFrom utils read.csv
evt.write <- function (segments, file="", header=NULL) {
    
    # Later code assumes that there are multiple segments
    if (is.data.frame(segments))
        segments <- list(segments)
    
    if (is.null(header)) {

        header_string <- "5
File
Acquire	Z:\\nonsense.dat	0	
Sweeps
1	0	0	0	0	0	
2	0 0 0 0 0 
Segments
1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	
0	0	0	0	0	0	
Events\r"
        ## Use DOS line endings
        header_string <- gsub("\n", "\r\n", header_string)

    } else {
        header_string <- header
    }

    
    write(header_string, file) 

    for (i in 1:length(segments)) {

        segment <- segments[[i]]
        
        data <- evt.from_dwells(segment)

        times <- data$times

        times <- sprintf("%.8f", times)

        states <- data$states


        ## This forces a tab to be placed at the beginning
        col1 <- rep(i, length(times))
        col3 <- rep("0.000000E+000",length(times))
        col4 <- rep("0.000000E+000",length(times))
        col6 <- rep("0.000000E+000",length(times))
        
        data  <- data.frame(col1, times, col3, col4, states, col6)

        write.table(data, file, append=TRUE, sep="\t", col.names=FALSE, row.names=FALSE, eol="\r\n", quote = FALSE) 

    }
}








#' Calculate pulse lengths. Converts transition times to dwell durations.
#'
#' @param tables Either a single table or a list of tables with columns "states" and "times"
#' @return A segment or a list of segments with one less row, where each row
#' represents pulse in state 0 (closed dwell) of duration 0.51231, instead
#' of the time at which the state transitioned.
#'
#' @examples
#' 
#' # Example rows
#' # states    dwells
#' # 0         0.016010
#'
#' \dontrun{
#' segments <- evt.to_dwells(tables)
#' }
#' @export
evt.to_dwells <- function(tables) {

    if (!is.data.frame(tables)) {

        return(lapply(tables, evt.to_dwells))
        
    } else {

        table <- tables

        states <- table$states
        times  <- table$times
        
        ## Calculate the durations, the last one gets thrown away
        dwells <- diff(times)

        ## remove the first pulse, and ignore the trailing end-dwell
        states <- states[1:length(states)-1]

        ## NOTE: the use of "name" here, is kinda an abuse.
        if (!is.null(segment.name(table))) {
            segment <- segment.create(states, dwells, name=segment.name(table))
        } else {
            segment <- segment.create(states, dwells)
        }
        
        return(segment)
        
    }

}






#' Converts dwell durations to absolute transition times
#'
#' @param segments A segment or multiple segemtns
#' @return A dataframe or multiple dataframes of states and transition times
#' @examples
#' 
#' \dontrun{
#' tables <- evt.from_dwells(segments)
#'
#' evt.write(tables, file="output.evt")
#' 
#' }
#' @export
evt.from_dwells <- function(segments) {

    if (!is.data.frame(segments)) {

        return(lapply(segments, evt.from_dwells))

    } else {

        segment <- segments
        
        times <- diffinv(segment$dwells)

        ## The last dwell is always a 1, meaning at the end you transition to 0.
        states <- append(segment$states, 0)

        data  <- data.frame(states, times)

        return(data)
        
    }
}
