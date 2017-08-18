#' Reformat the digits of data before writing to disk.
#' Should ask about this. At the moment this has a hard-coded
#' accuracy, because I don't understand significant figures.
#'
#' Writes DOS line endings. 
#'
#' @param segment Segment with $dwells and $states
#' @param filename Filename to write to
#' @examples
#' write.dwt(segment, "file-test.dwt")
#' @export
write.dwt <- function(segment, filename, seg=1) {

    header <- sprintf("Segment: %d   Dwells: %d", seg, length(segment$states))

    write(header, filename) 

    states <- segment$states
    dwells <- segment$dwells

    dwells <- sprintf("%.6f", dwells)

    # This forces a tab to be placed at the beginning
    junk <- rep("",length(dwells))
    
    data  <- data.frame(junk,states, dwells)
    
    write.table(data, filename, append=TRUE, sep="\t", col.names=FALSE, row.names=FALSE, eol="\n", quote = FALSE) 
    
}




#' Read a .dwt file. Result is a "segment", which is a
#' dataframe extra data. See "segment" for more details.
#'
#' @param filename Filename to read from
#' @examples
#' seg <- read.dwt(segment, "file-test.dwt")
#' @export
read.dwt <- function (filename) {

    # load lines
    FileInput <- readLines(filename) 

    header <- FileInput[[1]]

    seg <- strtoi(sub("Segment: *([0-9]*).*",  "\\1", header, perl=TRUE))
    # dwells <- sub(".*Dwells: *([0-9]*).*", "\\1", header, perl=TRUE)

    table <- read.csv(filename, skip=1, sep="\t",header=FALSE)

    dwells <- table[,2]
    states <- table[,1]

    return(segment(states, dwells, seg=seg, start_time=0, name=get_basename(filename)))
    
}





get_basename <- function(filename) {
    ### Remove the .dwt from filename
    substr(basename(filename), 1, nchar(basename(filename)) - 4) 
}


