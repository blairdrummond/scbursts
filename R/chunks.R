#' Split record at long pauses, dividing the record
#' into multiple -shorter- bursts.
#'
#' @param table Table with $states and $dwells
#' @param t_crit Critical time (us) at which to divide bursts
#' @return A FUNCTION!!! chunk that returns the chunk by its index
#' @examples
#' chunk <- chunk_bursts(table, 14.77155587)
#' head(chunk(1))
#' >     states      dwells
#' > 427      0 15.16625000
#' > 428      1  0.31105000
#' > 429      0  0.01289401
#' > 430      1  0.04823000
#' > 431      0  0.04160000
#' > 432      1  0.14415000
#' 
#' @export
chunk_bursts <- function(table, t_crit) {

    ### Find all breakpoints
    break_func <- function(row) {
        row$dwells > t_crit & row$state == 0
    }
    pauses <- break_func(table)
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
                # n is the n+1^nth index of the table, and i the i+1^st
                # So (n+1:i-1) in the table -> (n:i-2)
                return (n:(i-2))   
            }
        }
        return(NULL)
    }
    
    ### Create list of chunks (indices only)
    chunk_selectors <- Filter(Negate(is.null), sapply(1:length(pauses), find_next))


    ### Select the chunks using the indices
    chunk <- function(index) {


        if (index > length(chunk_selectors) || index <= 0) {
            warning("There aren't that many chunks. Returning NULL")
            return(NULL)
        }
        
        
        df <- table[unlist(chunk_selectors[index]),]
        attr(df, "segment") <- index
        return(df)
    }
    
    attr(chunk, "length")   <- length(chunk_selectors)
    attr(chunk, "filename") <- attr(table, "filename")
    
    return(chunk)

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
write_chunks_to_file <- function (chunks, directory="bursts") {

    if (is.null(attr(chunks, "filename"))) {
        file_prefix <- "burst"
    } else {
        file_prefix <- attr(chunks, "filename")
    }
    
    len <- ceiling(log10(attr(chunks, "length")))
    str <- sprintf("%s/%%s/%s-%%0%dd.dwt", directory, file_prefix, len)

    time <- format(Sys.time(), "%F-%H-%M")
    dir.create("bursts")
    dir.create(file.path("bursts", time))
    for (i in 1:attr(chunks, "length")) {
        file_name <- sprintf(str, time, i)
        write.dwt(chunks(i), file_name, segment=attr(chunks(i),"segment"))
    }

    return (time)
    
}








