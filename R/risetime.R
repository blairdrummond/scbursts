#' Undo the effect of the guassian filter
#' See section 4.1.1 of Colquhoun and Sigworth,
#' "Fitting and Analysis of Single-Channel records"
#'
#' NOTE: This is potentially problematic, in that this unfiltering
#' lengthens every state. A less naïve algorithm would take into
#' account the infulence of the surroundings, as they impact the
#' effects of the filter.
#'
#' @param Tr Rise time of the filter in (us)
#' @param table table with $states and $dwells to correct.
#' @return Table with corrected risetimes.
#' @examples
#' table <- correct_risetime(14.77155587, table)
#' @export
correct_risetime <- function(Tr, table) {


    ### Tr = rise time (in us!!)
    ### 
    ### Examples:
    ### 
    ###     14.72596465   (CDC match)
    ###     14.77155587   (Narges match)
    ###     14.748725     (HighRes Paper match)
    ###     0.484633      (SIM no filter match)
    ###     4.92258194    (PC best match)
    ###     35.0052278    (Old match)


    Trm = Tr / 1000        
    a1  = 0.5382 * Trm
    a2  = 0.837  * Trm**(-2)
    a3  = 1.120  * Trm**(-3)

    rescale <- function(T) {
        ### undo the effect of a guassian filter to one time interval
        T <- 1000 * T
	return( T + a1 * exp(- T / a1 - a2 * T**2 - a3 * T**3) )
    }


    ### This is somewhat problematic, as the length of the whole file increases.
    ### In the future, it's worth exploring more elegant solutions to this

    dwells <- table$dwells
    table$dwells <- rescale(dwells)
    
    return(table)

}
