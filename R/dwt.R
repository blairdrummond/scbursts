#' Reformat the digits of data before writing to disk.
#'
#' Writes DOS line endings. 
#'
#' @param segment segment with $dwells and $states
#' @param file Filename to write to
#' @param seg Segment to write in .dwt header.
#' @examples
#' \dontrun{
#' dwt.write(segment, "file-test.dwt")
#' }
#' @export
#' @importFrom utils write.table
dwt.write <- function(segment, file="", seg=1) {

    header <- sprintf("Segment: %d   Dwells: %d", seg, length(segment$states))

    write(header, file) 

    states <- segment$states
    dwells <- segment$dwells

    dwells <- sprintf("%.6f", dwells)

    # This forces a tab to be placed at the beginning
    junk <- rep("",length(dwells))
    
    data  <- data.frame(junk,states, dwells)
    
    write.table(data, file, append=TRUE, sep="\t", col.names=FALSE, row.names=FALSE, eol="\n", quote = FALSE) 
    
}




#' Read a .dwt file. Result is a "segment", which is a
#' dataframe extra data. See "segment" for more details.
#'
#' @param filename Filename to read from
#' @examples
#' \dontrun{
#' seg <- dwt.read(segment, "file-test.dwt")
#' }
#' @export
#' @importFrom utils read.csv
dwt.read <- function (filename) {

    # load lines
    FileInput <- readLines(filename) 

    header <- FileInput[[1]]

    seg <- strtoi(sub("Segment: *([0-9]*).*",  "\\1", header, perl=TRUE))
    # dwells <- sub(".*Dwells: *([0-9]*).*", "\\1", header, perl=TRUE)

    table <- read.csv(filename, skip=1, sep="\t",header=FALSE)

    # NOTE: Column 1 is empty, and thats how we get the spacing right.
    dwells <- table[,3]
    states <- table[,2]

    return(segment.create(states, dwells, seg=seg, start_time=0, name=util.basename(filename)))
    
}







#' Read bursts from a folder.
#'
#' @param folder list of dataframes corresponding to bursts
#' @return A pair (bursts,gaps), where the bursts are segments
#' starting and ending in an open dwell, and gaps is a vector of 0s which sit
#' inbetween the bursts. There will be n bursts and n-1 gaps.
#' @examples
#' \dontrun{
#' pair <- dwt.read_bursts("bursts/60uM-2017-08-19-19-35")
#' bursts <- pair$bursts
#' gaps <- pair$gaps
#' }
#' @export
#' @importFrom utils read.table
dwt.read_bursts <- function (folder) {

    filenames <- sort(list.files(folder, pattern="*.dwt", full.names = TRUE))
    bursts <- lapply(filenames, dwt.read)


    gaps <- file.path(folder,"gaps.csv")
    if (file.exists(gaps)) {
        gaps <- read.table(gaps)$V1  # read as vector
    } else {
        warning("No Gaps File Found! Starting times will not be accurate!")
        gaps <- rep(0, length(bursts)) # otherwise set them to zero
    }
    
    bursts <- bursts.start_times_update(bursts, gaps)

    return(list(bursts=bursts,gaps=gaps))

}





#' Write all the bursts to dwt. They will be placed in
#' a timestamped subfolder of bursts/
#'
#' All bursts will be written out as .dwt files, labeled
#' in temporal order.
#'
#' @param bursts list of dataframes corresponding to bursts
#' @param gaps vector of gaps to write as csv (default is to try to reconstruct from bursts)
#' @param directory folder to create for bursts
#' @param filename label for the files
#' @param timestamp Put the date and time in the folder-name
#' @return Name of folder containing contents.
#' @examples
#' \dontrun{
#' dwt.write_bursts(bursts)
#' }
#' @export
#' @importFrom utils write.table
dwt.write_bursts <- function (bursts, gaps=NULL, directory="bursts", filename="burst", timestamp=TRUE) {


    ### Write the bursts
    if (!is.null(segment.name(bursts[[1]]))) {
        filename <- segment.name(bursts[[1]])
    }

    len <- ceiling(log10(length(bursts)))
    str <- sprintf("%s-%%0%dd.dwt", filename, len)

    if (timestamp) {
        time <- format(Sys.time(), "%F-%H-%M")
        subfolder <- file.path("bursts", paste(filename,time,sep='-'))
    } else {
        subfolder <- file.path("bursts", filename)
    }

    
    dir.create("bursts")
    dir.create(subfolder)

    for (i in 1:length(bursts)) {
        filename <- file.path(subfolder, sprintf(str, i))
        dwt.write(segment=bursts[[i]], file=filename, seg=i)
    }


    ### Add a .csv containing all of the gaps.
    if (is.null(gaps)) {
        gaps <- bursts.get_gaps(bursts)
        write.table(gaps, file=file.path(subfolder,"gaps.csv"), col.names = FALSE, row.names = FALSE)
    }


    return (subfolder)
    
}
