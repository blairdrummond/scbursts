#' Split record at long pauses, dividing the record
#' into multiple -shorter- bursts.
#'
#' @param segment Segment with $states and $dwells
#' @param t_crit Critical time (us) at which to divide bursts
#' @return A list of segments, one per burst.
#' @examples
#'
#' # Note that lists are accessed with [[i]], not [i].
#' 
#' chunks <- chunk_bursts(segment, 14.77155587)
#' head(chunks[[1]])
#' >     states      dwells
#' > 427      0 15.16625000
#' > 428      1  0.31105000
#' > 429      0  0.01289401
#' > 430      1  0.04823000
#' > 431      0  0.04160000
#' > 432      1  0.14415000
#' 
#' @export
chunk_bursts <- function(segment, t_crit) {

    ### Find all breakpoints
    break_func <- function(row) {
        row$dwells > t_crit & row$state == 0
    }
    pauses <- break_func(segment)
    pauses <- c(c(TRUE),pauses,c(TRUE)) ### Causes first and last chunk to be included


    ### Turn breakpoints into selected regions (indices only)
    find_next <- function(n) {

        ## Not on a pulse
        if (!isTRUE(pauses[n])) {
            return (NULL)
        }
            
        if (n == length(pauses)) {
            return(NULL)
        }
       
        for (i in (n+1):(length(pauses))) {
            if (pauses[i]) {
                # n is the n+1^nth index of the segment, and i the i+1^st
                # So (n+1:i-1) in the segment -> (n:i-2)
                return (n:(i-2))   
            }
        }
        return(NULL)
    }
    
    ### Create list of chunks (indices only)
    chunk_selectors <- Filter(Negate(is.null), sapply(1:length(pauses), find_next))


    ### Select the chunks using the indices
    chunks <- list()

    starting_time <- function(i) {
        if (i == 1) {
            return(0)
        } else {
            t <- segment.start_time(chunks[[i-1]]) + sum(chunks[[i-1]]$dwells)
            return(t)
        }
    }

    for (i in seq_along(chunk_selectors)) {

        df <- segment[unlist(chunk_selectors[i]),]

        s <- segment.create(
            df$states,
            df$dwells,
            seg=i,
            start_time=starting_time(i),
            name=segment.name(segment)
        )
        
        chunks[[i]] <- s
    }
    
    return(chunks)

}




#' Write all the bursts to files. They will be placed in
#' a timestamped subfolder of bursts/
#'
#' All bursts will be written out as .dwt files, labeled
#' in temporal order.
#'
#' @param chunks list of dataframes corresponding to bursts
#' @return Timestamp
#' @examples
#' write_chunks_to_file(chunks)
#' @export
write_chunks_to_file <- function (chunks, directory="bursts", filename="burst") {

    if (!is.null(segment.name(chunks[[1]]))) {
        filename <- segment.name(chunks[[1]])
    }

    len <- ceiling(log10(length(chunks)))
    str <- sprintf("%s-%%0%dd.dwt", filename, len)
    time <- format(Sys.time(), "%F-%H-%M")
    subfolder <- file.path("bursts", paste(filename,time,sep='-'))
    
    dir.create("bursts")
    dir.create(subfolder)

    for (i in 1:length(chunks)) {
        filename <- file.path(subfolder, sprintf(str, i))
        write.dwt(segment=chunks[[i]], filename=filename, seg=i)
    }

    return (time)
    
}

