#' Reformat the digits of data before writing to disk.
#' Should ask about this. At the moment this has a hard-coded
#' accuracy, because I don't understand significant figures.
#'
#' Writes DOS line endings. 
#'
#' @param segment Segment with $dwells and $states
#' @param filename Filename to write to
#' @examples
#' dwt.write(segment, "file-test.dwt")
#' @export
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
#' seg <- dwt.read(segment, "file-test.dwt")
#' @export
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







#' Read chunks from a folder.
#'
#' @param folder list of dataframes corresponding to bursts
#' @return A pair (chunks,breaks), where the chunks are segments
#' starting and ending in 1 states, and breaks is a vector of 0s which sit
#' inbetween the bursts. There will be n chunks and n-1 breaks.
#' @examples
#' pair <- dwt.read_bursts("bursts/60uM-2017-08-19-19-35")
#' chunks <- pair$chunks
#' breaks <- pair$breaks
#' @export
dwt.read_bursts <- function (folder) {

    filenames <- sort(list.files(folder, pattern="*.dwt", full.names = TRUE))
    chunks <- lapply(filenames, dwt.read)


    breaks <- file.path(folder,"breaks.csv")
    if (file.exists(breaks)) {
        breaks <- read.table(breaks)$V1  # read as vector
    } else {
        warning("No Breaks File Found! Starting times will not be accurate!")
        breaks <- rep(0, length(chunks)) # otherwise set them to zero
    }
    
    chunks <- bursts.start_times_update(chunks, breaks)

    return(list(chunks=chunks,breaks=breaks))

}





#' Write all the bursts to dwt. They will be placed in
#' a timestamped subfolder of bursts/
#'
#' All bursts will be written out as .dwt files, labeled
#' in temporal order.
#'
#' @param chunks list of dataframes corresponding to bursts
#' @return Name of folder containing contents.
#' @examples
#' dwt.write_chunks(chunks)
#' @export
dwt.write_bursts <- function (chunks, breaks=c(), directory="bursts", filename="burst", timestamp=TRUE) {


    ### Write the bursts
    if (!is.null(segment.name(chunks[[1]]))) {
        filename <- segment.name(chunks[[1]])
    }

    len <- ceiling(log10(length(chunks)))
    str <- sprintf("%s-%%0%dd.dwt", filename, len)

    if (timestamp) {
        time <- format(Sys.time(), "%F-%H-%M")
        subfolder <- file.path("bursts", paste(filename,time,sep='-'))
    } else {
        subfolder <- file.path("bursts", filename)
    }

    
    dir.create("bursts")
    dir.create(subfolder)

    for (i in 1:length(chunks)) {
        filename <- file.path(subfolder, sprintf(str, i))
        dwt.write(segment=chunks[[i]], file=filename, seg=i)
    }


    ### Add a .csv containing all of the breaks.
    if (length(breaks) != 0) {
        write.table(breaks, file=file.path(subfolder,"breaks.csv"), col.names = FALSE, row.names = FALSE)
    }

    return (subfolder)
    
}

