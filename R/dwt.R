#' Reformat the digits of data before writing to disk.
#' Should ask about this. At the moment this has a hard-coded
#' accuracy, because I don't understand significant figures.
#'
#' Writes DOS line endings. 
#'
#' @param table Table with $dwells and $states
#' @param filename Filename to write to
#' @examples
#' write.dwt(table, "file-test.dwt")
#' @export
write.dwt <- function(table, filename, segment=1) {

    header <- sprintf("Segment: %d   Dwells: %d\r", segment, length(table$states))

    write(header, filename) 

    states <- table$states
    dwells <- table$dwells

    dwells <- sprintf("%.6f", dwells)
    
    data  <- data.frame(states, dwells)
    
    write.table(data, filename, append=TRUE, sep="\t", col.names=FALSE, row.names=FALSE, eol="\r\n", quote = FALSE) 
    
}
