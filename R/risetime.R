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
#' @param units What unit the risetime is input in (defaults to seconds)
#' @return Segment with corrected risetimes.
#' @examples
#' \dontrun{
#' segment <- risetime.correct_gaussian(14.77155587, segment)
#' }
#' @export
risetime.correct_gaussian <- function(Tr, segment, units="s") {

    ### Computations done in ms (milliseconds)
    
    ### s > ms > us > ns by orders of 10^3
    
    if (units == "s") {         
        Trm = Tr * 1000
    } else if (units == "ms") {
        Trm = Tr
    } else if (units == "us") {
        Trm = Tr / 1000
    } else if (units == "ns") {
        Trm = Tr / 1000000
    } else {
        stop("units must be either 's', 'ms', 'us', or 'ns'")
    }

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

    a1  = 0.5382 * Trm
    a2  = 0.837  * Trm**(-2)
    a3  = 1.120  * Trm**(-3)

    rescale <- function(T) {
        ### undo the effect of a gaussian filter to one time interval
	return( T + a1 * exp(- T / a1 - a2 * T**2 - a3 * T**3) )
    }


    ### This is somewhat problematic, as the length of the whole file increases.
    ### In the future, it's worth exploring more elegant solutions to this

    dwells <- segment$dwells
    dwells <- dwells * 1000 # seconds to milliseconds
    dwells <- rescale(dwells)
    dwells <- dwells / 1000 # milliseconds to seconds
    segment$dwells <- dwells
    
    return(segment)

}
