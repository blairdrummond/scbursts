#' Split record at long pauses, dividing the record
#' into multiple -shorter- bursts.
#'
#' @param record Record with $states and $dwells
#' @param t_crit Critical time (us) at which to divide bursts
#' @return A pair (bursts,gaps), where the bursts are records
#' starting and ending in 1 states, and gaps is a vector of 0s which sit
#' inbetween the bursts. There will be n bursts and n-1 gaps.
#' @examples
#' \dontrun{
#' pair <- dwt.read_bursts("bursts/60uM-2017-08-19-19-35")
#' bursts <- pair$bursts
#' gaps <- pair$gaps
#' 
#' # Note that lists are accessed with [[i]], not [i].
#' 
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
bursts.separate_tcrit <- function(record, t_crit) {

    ### Find all gaps
    gap_func <- function(row) {
        row$dwells > t_crit & row$state == 0
    }
    pauses <- gap_func(record)
    pauses <- c(c(TRUE),pauses,c(TRUE)) ### Causes first and last burst to be included





    # Extract the gaps
    filter_gaps <- function(i) {

        if (i == 1) {
            return (NULL)
        } else if (i == length(pauses)) {
            return (NULL)
        }
        
        if (pauses[i]) {
            return(record$dwells[i])
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
                # n is the n+1^nth index of the record, and i the i+1^st
                # So (n+1:i-1) in the record -> (n:i-2)
                return (n:(i-2))   
            }
        }
        return(NULL)
    }
    
    ### Create list of bursts (indices only)
    burst_selectors <- Filter(Negate(is.null), sapply(1:length(pauses), find_next))



    ### Select the bursts using the indices
    burst <- function(i) {

        df <- record[unlist(burst_selectors[i]),]

        s <- record.create(
            df$states,
            df$dwells,
            rec=i,
            start_time=0,
            name=record.name(record)
        )

        return(s)
    }
    
    bursts <- lapply(seq_along(burst_selectors), burst)
    bursts <- bursts.start_times_update(bursts, gaps)
    


    return( list( bursts=bursts , gaps=gaps ))

}



#' Attach the meta-data to each record saying when it began.
#'
#' It interleaves the durations of the bursts and gaps, and
#' assigns the sum of those durations up to a point as the
#' starting time
#'
#' You probably won't ever have to call this directly.
#'
#' @param bursts List of records
#' @param gaps vector of gap times.
#' @return A list of records, one per burst, with updated start_times
#' @export
bursts.start_times_update <- function (bursts, gaps) {

    starting_time <- function(i) {
        if (i == 1) {
            return(0)
        } else {
            t <- record.start_time(bursts[[i-1]]) + sum(bursts[[i-1]]$dwells) + gaps[i-1]
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
#' @param bursts The list of records
#' @return A vector of gap times
#' @examples
#' \dontrun{
#' gaps <- bursts.get_gaps(bursts)
#' }
#' @export
bursts.get_gaps <- function (bursts) {


    start_times <- sapply(bursts, record.start_time)
    durations   <- sapply(bursts, record.duration)
    
    gaps <- diff(start_times) - durations[1:length(durations)-1]
    
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








#' From a list of bursts, extract those that interest you by
#' passing a selecting function. See the examples.
#'
#' @param bursts The list of all bursts
#' @param func A function of a record that returns either TRUE or FALSE
#' @param one_file TRUE or FALSE: Return a single file to disk, or a list of bursts.
#' The one_file will return a file with all filtered bursts zeroed out.
#' @return A shorter list of bursts OR if one_file is passed one record with zeros where the other bursts might have been originally. Defaults to FALSE.
#' @examples
#' \dontrun{
#' high_popen <- function (rec) {
#'
#'     record.popen(rec) > 0.7
#' 
#' }
#'
#' subset <- bursts.filter(high_popen, bursts)
#'
#' 
#' # To export to one .dwt file
#' subset_f <- bursts.filter(high_popen, bursts, one_file=TRUE)
#' }
#' @export
bursts.filter <- function (bursts, func, one_file=FALSE) {


    filtered <- Filter(func, bursts)
    
    if (!one_file) {
        return(filtered)
    }

    ## else

    gaps <- diff(sapply(filtered, record.start_time))
    lengths <- sapply(filtered, record.duration)

    ## this is the time following one burst preceding another
    gap_lengths <- gaps - lengths[1:length(lengths)-1]



    ##### We MIGHT be missing the first and last gap. #####

    ## Add the first gap (if necessary)
    start <- record.start_time(filtered[[1]])
    if (start != 0) {

        gap_lengths <- append(start, gap_lengths)

        interleave_gaps_first <- TRUE
        
    } else {

        interleave_gaps_first <- FALSE

    }
    


    ## Add the last gap (if necessary)
    last_burst    <-   bursts[[length(bursts)]]
    last_filtered <- filtered[[length(filtered)]]
    if (record.start_time(last_filtered) != record.start_time(last_burst)) {

        end <- record.start_time(last_burst) + record.duration(last_burst)
        
        len <- end - record.duration(last_filtered)
        
        gap_lengths <- append(gap_lengths, end)
        
    } 
    
 
    

    
    faux_record <- function (dwell) {
        record.create(c(0),c(dwell))
    }

    ## list of size one dataframes
    faux_recs <- lapply(gap_lengths, faux_record)


    ## https://stackoverflow.com/questions/16443260/interleave-lists-in-r
    if (interleave_gaps_first) {
        a <- faux_recs
        b <- filtered
    } else {
        a <- filtered
        b <- faux_recs
    }
    
    ## interleave the lists
    idx <- order(c(seq_along(a), seq_along(b)))
    super_list <- (c(a,b))[idx]

    
    ## super list is now a list of records - which are just dataframes.
    ## We're going to fold all these dataframes up into one big one.
    flat <- Reduce(rbind, super_list, data.frame())

    ## NOTE: I should probably be doing this in a better way
    attr(flat, "name") <- attr(bursts[[1]], "name")
    attr(flat, "rec")  <- 1
    attr(flat, "start_time")  <- 0

    return (flat)
    
}








#' Order a list of bursts by some function. For instance, popen.
#'
#' @param bursts The list of all bursts
#' @param func A function of a record that returns a numeric value
#' @param reverse By default, return in ascending order. Use reverse=TRUE to change that.
#' @return A list sorted by func. By default in ascending order (unless reversed)
#' @examples
#' \dontrun{
#' sorted <- bursts.sort(record.popen, bursts)
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
bursts.popens <- function (bursts) {sapply(bursts, record.popen)}





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
bursts.pcloseds <- function (bursts) {sapply(bursts, record.pclosed)}
