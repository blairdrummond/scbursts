#' Read a .evt file to a table
#'
#' @param filename The filename
#' @return A table with columns "states" and "times"
#' @examples
#' table <- read.evt("data/60uM.evt")
#'
#' # import some of the data included with the package
#' infile <- system.file("extdata", "60uM.evt", package = "uottawaionchannel")
#' table <- read.evt(infile)
#'
#' @export
read.evt <- function (filename) {
    
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
    skip_line <- grep("^Events$",FileInput)
  
    # Read everything past 'Events'
    table <- read.csv(filename, skip=skip_line, sep="\t",header=FALSE)
    
    # extract the needed columns
    states <- table[,5]
    times  <- table[,2]

    data  <- data.frame(states, times)

    attr(data, "filename") <- get_basename(filename)
    
    return(data)
}




get_basename <- function(filename) {
    ### Remove the .evt from filename
    substr(basename(filename), 1, nchar(basename(filename)) - 4) 
}
