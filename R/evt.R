#' Read a .evt file to a table
#'
#' @param filename The filename
#' @return A table with columns "states" and "times"
#' @examples
#' \dontrun{
#' table <- evt.read("data/60uM.evt")
#'
#' # import some of the data included with the package
#' infile <- system.file("extdata", "60uM.evt", package = "uottawaionchannel")
#' table <- evt.read(infile)
#'
#' }
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
    skip_line <- tail(grep("^Events$",FileInput), n=1)

    # Read everything past 'Events'
    table <- read.csv(filename, skip=skip_line, sep="\t",header=FALSE)
    
    # extract the needed columns
    states <- table[,5]
    times  <- table[,2]

    data  <- data.frame(states, times)

    attr(data, "name") <- util.basename(filename)
    
    return(data)
}






#' Write burst(s) to a .evt file
#'
#' @param segment A single segment to write to filename
#' @param filename The filename
#' @examples
#' \dontrun{
#'
#' evt.write(segment, file="60mc-2.evt")
#'
#' }
#' @export
#' @importFrom utils read.csv
evt.write <- function (segment, file="") {
    

    header_string <- "5
File
Acquire	Z:\\nonsense.dat	0	
Sweeps
1	0	0	0	0	0	
2	0 0 0 0 0 
Segments
1	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	
0	0	0	0	0	0	
Events
"

    write(header_string, file) 


    data <- evt.from_dwells(segment)

    times <- data$times
    states <- data$states

    # This forces a tab to be placed at the beginning
    col1 <- rep(1,length(times))
    col3 <- rep("0.000000E+000",length(times))
    col4 <- rep("0.000000E+000",length(times))
    col6 <- rep("0.000000E+000",length(times))
    
    data  <- data.frame(col1, times, col3, col4, states, col6)

    write.table(data, file, append=TRUE, sep="\t", col.names=FALSE, row.names=FALSE, eol="\n", quote = FALSE) 

}








#' Calculate pulse lengths
#'
#' Converts transition times to dwell durations
#'
#' @param table with columns "states" and "times"
#' @return A "segment" with one less row, where each row
#' represents pulse in state 0 (closed dwell) of duration 0.51231, instead
#' of the time at which the state transitioned.
#'
#' Also, the output table preserves the attributes
#'
#' See "segment" for more info
#' 
#' @examples
#' 
#' # Example rows
#' # states    dwells
#' # 0         0.016010
#'
#' \dontrun{
#' segment <- evt.to_dwells(table)
#' }
#' @export
evt.to_dwells <- function(table) {

    states <- table$states
    times  <- table$times
    
    # Calculate the durations, the last one gets thrown away
    dwells <- diff(times)
    
    # remove the first pulse, and ignore the trailing end-dwell
    states <- states[1:length(states)-1]
    
    # NOTE: the use of "name" here, is kinda an abuse.
    if (!is.null(segment.name(table))) {
        segment <- segment.create(states, dwells, name=segment.name(table))
    } else {
        segment <- segment.create(states, dwells)
    }

    return(segment)

}






#' Converts dwell durations to absolute transition times
#'
#' @param segment A segment
#' @return A dataframe of states and transition times
#' @examples
#' 
#' \dontrun{
#' table <- evt.from_dwells(segment)
#' }
#' @export
evt.from_dwells <- function(segment) {

    times <- diffinv(segment$dwells)

    ## The last dwell is always a 1, meaning at the end you transition to 0.
    states <- append(segment$states, 0)

    data  <- data.frame(states, times)

    return(data)

}
