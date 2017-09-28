#' Split segment at long pauses, dividing the segment
#' into multiple -shorter- segments (which are the bursts),
#' Along with the interburst closings, which are referred to as "gaps".
#'
#' @param segment Segment with $states and $dwells
#' @param t_crit Critical time (us) at which to divide bursts
#' @return bursts. Which is a list of segments
#' starting and ending in 1 states (open dwell)
#' @examples
#' \dontrun{
#' # Splitting segment into smaller segments 
#' bursts <- bursts.defined_by_tcrit(record_c , 0.1)
#' 
#' # Note that you need two brackets to access list elements.
#' # Also note that each burst begins and ends with an open dwell
#' # The gaps seperating them are the elements of gaps.
#' head(bursts[[11]])
#' >     states      dwells
#' > 427      0 15.16625000
#' > 428      1  0.31105000
#' > 429      0  0.01289401
#' > 430      1  0.04823000
#' > 431      0  0.04160000
#' > 432      1  0.14415000
#' }
#' @export
bursts.defined_by_tcrit <- function(segment, t_crit) {

    ### Find all gaps
    gap_func <- function(row) {
        row$dwells > t_crit & row$state == 0
    }
    pauses <- gap_func(segment)
    pauses <- c(c(TRUE),pauses,c(TRUE)) ### Causes first and last burst to be included




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

    gaps <- Filter(Negate(is.null), sapply(1:length(pauses),filter_gaps))
    gaps <- unlist(gaps, use.names=FALSE)

    
    
    ### Turn gaps into selected regions (indices only)
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
    
    ### Create list of bursts (indices only)
    burst_selectors <- Filter(Negate(is.null), sapply(1:length(pauses), find_next))



    ### Select the bursts using the indices
    burst <- function(i) {

        df <- segment[unlist(burst_selectors[i]),]

        s <- segment.create(
            df$states,
            df$dwells,
            seg=i,
            start_time=0,
            name=segment.name(segment)
        )

        return(s)
    }
    
    bursts <- lapply(seq_along(burst_selectors), burst)
    bursts <- bursts.start_times_update(bursts, gaps)
    
    return(bursts)

}



#' YOU PROBABLY WON'T EVER HAVE TO CALL THIS DIRECTLY.
#' 
#' Attach the meta-data to each segment saying when it began.
#'
#' It interleaves the durations of the bursts and gaps, and
#' assigns the sum of those durations up to a point as the
#' starting time
#'
#' @param bursts List of segments
#' @param gaps vector of gap times.
#' @return A list of segments, one per burst, with updated start_times
#' @export
bursts.start_times_update <- function (bursts, gaps) {

    starting_time <- function(i) {
        if (i == 1) {
            return(0)
        } else {
            t <- segment.start_time(bursts[[i-1]]) + sum(bursts[[i-1]]$dwells) + gaps[i-1]
            return(t)
        }
    }

    ### CANNOT BE PARALLELIZED!
    for (i in 1:length(bursts)) {
        ### NOTE: Probably should create an actual setter method
        attr(bursts[[i]],"start_time") <- starting_time(i)
    }

    return(bursts)

}




#' Extract vector of gaps from the bursts
#'
#' This is done using the start_time attribute, which
#' is mostly hidden in the data.
#'
#' (The gaps at the ends may have length 0)
#'
#'
#' ================ Bursts =================
#' 
#'       1      2     3   4   5   6   7
#'      |||   |||||   |   |   |   |   |
#' _____|||___|||||___|___|___|___|___|_____
#'   1      2       3   4   5   6   7    8
#'
#' ================= Gaps ==================
#'
#'
#'
#' @param bursts The list of segments
#' @return A vector of N+1 gaps for N bursts times
#' @examples
#' \dontrun{
#' gaps <- bursts.get_gaps(bursts)
#' }
#' @export
bursts.get_gaps <- function (bursts, end_time=-1) {

    start_times <- sapply(bursts, segment.start_time)
    durations   <- sapply(bursts, segment.duration)

    diff_start <- diff(start_times)
    head_durations <- durations[1:length(durations)-1]
    
    gaps <- diff_start - head_durations
    
}


















#' Remove the first and last burst from the list
#'
#' @param bursts The list of all bursts
#' @return A shorter list of bursts
#' @examples
#'
#' \dontrun{
#' bursts <- bursts.remove_first_and_last(bursts)
#' }
#' 
#' @export
bursts.remove_first_and_last <- function (bursts) {
    bursts[2:length(bursts)-1]
}






#' From a list of segments, return the concatenated 
#' segment containing all bursts.
#'
#' Inverse of functions like bursts.defined_by_tcrit
#'
#' @param bursts The list of all bursts
#' @return The segment containing all bursts.
#' @examples
#' \dontrun{
#'
#' record <- bursts.recombine(bursts)
#' 
#' }
#' @export
bursts.recombine <- function (bursts) {

    ## This is a silly way to do it, but it works!

    all <- function (x) { TRUE }

    return ( bursts.select(bursts, all, one_file=TRUE) )

}





#' From a list of bursts, extract those that interest you by
#' passing a selecting function. See the examples.
#'
#' @param bursts The list of all bursts
#' @param func A function of a segment that returns either TRUE or FALSE
#' @param one_file TRUE or FALSE: Return a single file to write to disk, or a list of bursts.
#' The one_file will return a file with all unselected bursts zeroed out.
#' @return A shorter list of bursts OR if one_file is passed one segment with zeros where the other bursts might have been originally. Defaults to FALSE.
#' @examples
#' \dontrun{
#' high_popen <- function (seg) {
#'
#'     segment.popen(seg) > 0.7
#' 
#' }
#'
#' subset <- bursts.select(bursts, high_popen)
#'
#' 
#' # To export to one .dwt file
#' subset_f <- bursts.select(bursts, high_popen, one_file=TRUE)
#' }
#' @export
bursts.select <- function (bursts, func, one_file=FALSE) {

    ## "Filter" is ambiguous in the terriory of ion-channel analysis, and so
    ## it's prefereable to use "select" instead.

    filtered <- Filter(Negate(func), bursts)
    
    if (!one_file) {
        return(filtered)
    }

    gaps <- bursts.get_gaps(filtered)


    ##### We MIGHT be missing the first and last gap. #####

    ## Add the first gap (if necessary)
    start <- segment.start_time(filtered[[1]])
    if (start != 0) {

        gaps <- append(start, gaps)

        interleave_gaps_first <- TRUE
        
    } else {

        interleave_gaps_first <- FALSE

    }


    


    ## Add the last gap (if necessary)
    last_burst    <-   bursts[[length(bursts)]]
    last_filtered <- filtered[[length(filtered)]]
    if (segment.start_time(last_filtered) != segment.start_time(last_burst)) {

        end <- segment.start_time(last_burst) + segment.duration(last_burst)
        
        len <- end - (segment.duration(last_filtered) + segment.start_time(last_filtered))
        
        gaps <- append(gaps, len)
        
    } 
    
    
    

    
    faux_segment <- function (dwell) {
        segment.create(c(0),c(dwell))
    }

    ## list of size one dataframes
    faux_segs <- lapply(gaps, faux_segment)


    ## https://stackoverflow.com/questions/16443260/interleave-lists-in-r
    if (interleave_gaps_first) {
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
    attr(flat, "name") <- attr(bursts[[1]], "name")
    attr(flat, "seg")  <- 1
    attr(flat, "start_time")  <- 0

    return (flat)
    
}








#' Order a list of bursts by some function. For instance, popen.
#'
#' @param bursts The list of all bursts
#' @param func A function of a segment that returns a numeric value
#' @param reverse By default, return in ascending order. Use reverse=TRUE to change that.
#' @return A list sorted by func. By default in ascending order (unless reversed)
#' @examples
#' \dontrun{
#' sorted <- bursts.sort(segment.popen, bursts)
#' }
#' @export
bursts.sort <- function (bursts, func, reverse=FALSE) {

    vec_order <- order(sapply(bursts, func))
    
    sorted <- bursts[vec_order]

    if (reverse) {
        sorted <- rev(sorted)
    }

    return (sorted)
    
}

#' Return popens of every burst.
#'
#'
#' @param bursts The list of all bursts
#' @return The popen values
#' @examples
#' \dontrun{
#' popens <- bursts.popens(bursts)
#' hist(popens)
#' }
#' @export
bursts.popens <- function (bursts) {sapply(bursts, segment.popen)}





#' Return pcloseds of every burst.
#'
#'
#' @param bursts The list of all bursts
#' @return The pclosed values
#' @examples
#' \dontrun{
#' pcloseds <- bursts.popens(bursts)
#' hist(pcloseds)
#' }
#' @export
bursts.pcloseds <- function (bursts) {sapply(bursts, segment.pclosed)}
