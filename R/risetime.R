#' Undo the effect of the gaussian filter
#' See section 4.1.1 of Colquhoun and Sigworth,
#' "Fitting and Analysis of Single-Channel segments"
#'
#' NOTE: This is potentially problematic, in that this unfiltering
#' lengthens every dwell. A less naive algorithm would take into
#' account the infulence of the surroundings, as they impact the
#' effects of the filter.
#'
#' @param Tr Rise time of the filter in (us)
#' @param segment segment with $states and $dwells to correct.
#' @return Segment with corrected risetimes.
#' @examples
#' \dontrun{
#' segment <- risetime.correct_gaussian(14.77155587, segment)
#' }
#' @export
risetime.correct_gaussian <- function(Tr, segment) {


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
        ### undo the effect of a gaussian filter to one time interval
        T <- 1000 * T
	return( T + a1 * exp(- T / a1 - a2 * T**2 - a3 * T**3) )
    }


    ### This is somewhat problematic, as the length of the whole file increases.
    ### In the future, it's worth exploring more elegant solutions to this

    dwells <- segment$dwells
    segment$dwells <- rescale(dwells)
    
    return(segment)

}
