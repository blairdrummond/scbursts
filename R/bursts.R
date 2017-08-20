#' Split record at long pauses, dividing the record
#' into multiple -shorter- bursts.
#'
#' @param segment Segment with $states and $dwells
#' @param t_crit Critical time (us) at which to divide bursts
#' @return A pair (chunks,breaks), where the chunks are segements
#' starting and ending in 1 states, and breaks is a vector of 0s which sit
#' inbetween the bursts. There will be n chunks and n-1 breaks.
#' @examples
#' pair <- dwt.read_bursts("bursts/60uM-2017-08-19-19-35")
#' chunks <- pair$chunks
#' breaks <- pair$breaks
#' 
#' # Note that lists are accessed with [[i]], not [i].
#' 
#' head(chunks[[11]])
#' >     states      dwells
#' > 427      0 15.16625000
#' > 428      1  0.31105000
#' > 429      0  0.01289401
#' > 430      1  0.04823000
#' > 431      0  0.04160000
#' > 432      1  0.14415000
#' 
#' @export
bursts.separate_tcrit <- function(segment, t_crit) {

    ### Find all breakpoints
    break_func <- function(row) {
        row$dwells > t_crit & row$state == 0
    }
    pauses <- break_func(segment)
    pauses <- c(c(TRUE),pauses,c(TRUE)) ### Causes first and last chunk to be included





    # Extract the gaps
    filter_gaps <- function(i) {

        if (i == 1) {
            return (NULL)
        } else if (i == length(pauses)) {
            return (NULL)
        }
        
        if (pauses[i]) {
            return(segment$dwells[i])
        } else {
            return (NULL)
        }
        
    }

    breaks <- Filter(Negate(is.null), sapply(1:length(pauses),filter_gaps))
    breaks <- unlist(breaks, use.names=FALSE)

    
    
    
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
    chunk <- function(i) {

        df <- segment[unlist(chunk_selectors[i]),]

        s <- segment.create(
            df$states,
            df$dwells,
            seg=i,
            start_time=0,
            name=segment.name(segment)
        )

        return(s)
    }
    
    chunks <- lapply(seq_along(chunk_selectors), chunk)
    chunks <- bursts.start_times_update(chunks, breaks)
    


    return( list( chunks=chunks , breaks=breaks ))

}



#' Take an ORDERED list of bursts, and find the starting
#' time of every 
#' into multiple -shorter- bursts.
#'
#' @param segment Segment with $states and $dwells
#' @param t_crit Critical time (us) at which to divide bursts
#' @return A list of segments, one per burst.
#' @examples
#'
#' # Note that lists are accessed with [[i]], not [i].
#' 
#' chunks <- bursts.separate_tcrit(segment, 14.77155587)
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
bursts.start_times_update <- function (chunks, breaks) {

    starting_time <- function(i) {
        if (i == 1) {
            return(0)
        } else {
            t <- segment.start_time(chunks[[i-1]]) + sum(chunks[[i-1]]$dwells) + breaks[i-1]
            return(t)
        }
    }

    ### CANNOT BE PARALLELIZED!
    for (i in 1:length(chunks)) {
        ### NOTE: Probably should create an actual setter method
        attr(chunks[[i]],"start_time") <- starting_time(i)
    }

    return(chunks)

}



#' Remove the first and last burst from the list
#'
#' @param chunks The list of all bursts
#' @return A shorter list of bursts
#' @examples
#'
#' chunks <- bursts.remove_first_and_last(chunks)
#' 
#' @export
bursts.remove_first_and_last <- function (chunks) {
    chunks[2:length(chunks)-1]
}
