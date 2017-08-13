#' Split record at long pauses, dividing the record
#' into multiple -shorter- bursts.
#'
#' @param table Table with $states and $dwells
#' @param t_crit Critical time (us) at which to divide bursts
#' @return list of columns of dataframes (one per row)
#' @examples
#' chunks <- chunk_bursts(table, 14.77155587)
#' @export
chunk_bursts <- function(table, t_crit) {


    ### Find all breakpoints
    break_func <- function(row) {
        row$dwells > t_crit & row$state == 0
    }
    pauses <- break_func(table)


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
                return (n:i)
            }
        }
        return(NULL)
    }
    
    ### Create list of chunks (indices only)
    chunk_selectors <- Filter(Negate(is.null), sapply(1:length(pauses), find_next))


    ### Select the chunks using the indices
    chunk <- function(sel) {
        data.frame(table[sel,,])
    }
    chunks <- t(sapply(chunk_selectors, chunk)) # Need to transpose (?)
    
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
write_chunks_to_file <- function (chunks) {

    len <- floor(log10(length(chunks)))
    str <- sprintf("bursts/%%s/%%s-%%0%dd.dwt", len)

    print(str)
    
    time <- format(Sys.time(), "%F-%H-%M")
    dir.create("bursts")
    dir.create(file.path("bursts", time))
    for (i in 1:round(length(chunks)/2)) {
        file_name <- sprintf(str, time, file_prefix, i)
        write.dwt(chunks[i,], file_name)
    }

    return (time)
    
}
