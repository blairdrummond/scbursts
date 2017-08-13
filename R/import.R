# infile="data/real data.evt"
# infile="data/60uM.evt"


#' Read a .evt file to a table
#'
#' @param infile The filename
#' @return A table with columns "states" and "times"
#' @examples
#' table <- read.evt("data/60uM.evt")
#' @export
read.evt <- function (infile) {
    
    ### Assumes infile has the format

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
    FileInput <- readLines(infile) 

    # Jump to where the data starts
    skip_line <- grep("^Events$",FileInput)
  
    # Read everything past 'Events'
    table <- read.csv(infile, skip=skip_line, sep="\t")
    
    # extract the needed columns
    states <- table[,5]
    times  <- table[,2]

    data  <- data.frame(states, times)
    
    return(data)
}




