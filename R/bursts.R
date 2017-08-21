#' Split record at long pauses, dividing the record
#' into multiple -shorter- bursts.
#'
#' @param segment Segment with $states and $dwells
#' @param t_crit Critical time (us) at which to divide bursts
#' @return A pair (chunks,breaks), where the chunks are segments
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




#' Extract vector of breaks from the chunks
#'
#' This is done using the start_time attribute, which
#' is mostly hidden in the data.
#'
#' @param chunks The list of segments
#' @return A vector of break times
#' @examples
#' 
#' breaks <- bursts.get_breaks(chunks)
#' 
#' @export
bursts.get_breaks <- function (chunks) {


    start_times <- sapply(chunks, segment.start_time)
    durations   <- sapply(chunks, segment.duration)
    
    breaks <- diff(start_times) - durations[1:length(durations)-1]
    
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








#' From a list of bursts, extract those that interest you by
#' passing a selecting function. See the examples.
#'
#' @param chunks The list of all bursts
#' @param func A function of a segment that returns either TRUE or FALSE
#' @return A shorter list of bursts OR if one_file is passed one segment
#' with zeros where the other bursts might have been originally.
#' @examples
#'
#' high_popen <- function (seg) {
#'
#'     segment.popen(seg) > 0.7
#' 
#' }
#'
#' length(chunks)
#' > 733
#'
#' subset <- bursts.filter(high_popen, chunks)
#'
#' length(subset)
#' > 721
#'
#' 
#' # To export to one .dwt file
#' subset_f <- bursts.filter(high_popen, chunks, one_file=TRUE)
#' 
#' 
#' @export
bursts.filter <- function (chunks, func, one_file=FALSE) {


    filtered <- Filter(func, chunks)
    
    if (!one_file) {
        return(filtered)
    }

    ## else

    gaps <- diff(sapply(filtered, segment.start_time))
    lengths <- sapply(filtered, segment.duration)

    ## this is the time following one burst preceding another
    break_lengths <- gaps - lengths[1:length(lengths)-1]



    ##### We MIGHT be missing the first and last gap. #####

    ## Add the first gap (if necessary)
    start <- segment.start_time(filtered[[1]])
    if (start != 0) {

        break_lengths <- append(start, break_lengths)

        interleave_breaks_first <- TRUE
        
    } else {

        interleave_breaks_first <- FALSE

    }
    


    ## Add the last gap (if necessary)
    last_burst    <-   chunks[[length(chunks)]]
    last_filtered <- filtered[[length(filtered)]]
    if (segment.start_time(last_filtered) != segment.start_time(last_burst)) {

        end <- segment.start_time(last_burst) + segment.duration(last_burst)
        
        len <- end - segment.duration(last_filtered)
        
        break_lengths <- append(break_lengths, end)
        
    } 
    
 
    

    
    faux_segment <- function (dwell) {
        segment.create(c(0),c(dwell))
    }

    ## list of size one dataframes
    faux_segs <- lapply(break_lengths, faux_segment)


    ## https://stackoverflow.com/questions/16443260/interleave-lists-in-r
    if (interleave_breaks_first) {
        a <- faux_segs
        b <- filtered
    } else {
        a <- filtered
        b <- faux_segs
    }
    
    ## interleave the lists
    idx <- order(c(seq_along(a), seq_along(b)))
    super_list <- (c(a,b))[idx]

    
    ## super list is now a list of segments - which are just dataframes.
    ## We're going to fold all these dataframes up into one big one.
    flat <- Reduce(rbind, super_list, data.frame())

    ## NOTE: I should probably be doing this in a better way
    attr(flat, "name") <- attr(chunks[[1]], "name")
    attr(flat, "seg")  <- 1
    attr(flat, "start_time")  <- 0

    return (flat)
    
}








#' Order a list of bursts by some function. For instance, popen.
#'
#' @param chunks The list of all bursts
#' @param func A function of a segment that returns a numeric value
#' @return A list sorted by func. By default in ascending order (unless reversed)
#' @examples
#'
#' sorted <- bursts.sort(segment.popen, chunks)
#'
#' @export
bursts.sort <- function (chunks, func, reverse=FALSE) {

    vec_order <- order(sapply(chunks, func))
    
    sorted <- chunks[vec_order]

    if (reverse) {
        sorted <- rev(sorted)
    }

    return (sorted)
    
}
