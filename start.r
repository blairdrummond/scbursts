###  So far ...
### 
###    Load data
###    
###    Plot histograms in log / sqrt scale
### 
###  Todo ...
### 
###    Correct Dwell times !!! NOTE : this might go inbetween current steps !!!
### 
### 
 

# library("devtools")

# infile="data/real data.evt"
infile="files/60uM.evt"


file_prefix = "my-data"



###     args <- commandArgs(trailingOnly=TRUE)
###     
###     # test if there is at least one argument: if not, return an error
###     if (length(args)==0) {
###       stop("At least one argument must be supplied (input file).ect", call.=FALSE)
###     } else if (length(args)==1) {
###       # default output file
###       args[2] = "out.txt"
###     }
###     
###     
###     infile  <- args[1]
###     outfile <- args[2]


read.evt <- function (infile) {
    
    ### Assumes infile has the format

    ### blah blah blah
    ### 
    ### ... 
    ###
    ### Events
    ### 1	0.05027597	6.346019E-012	1.943602E-011	1	0.000000E+000
    ### 1	0.05032199	1.943602E-011	6.346019E-012	0	0.000000E+000
    ### 1	0.05035479	6.346019E-012	1.943602E-011	1	0.000000E+000
    ### 1	0.05090435	1.943602E-011	6.346019E-012	0	0.000000E+000
    ### 1	0.05092179	6.346019E-012	1.943602E-011	1	0.000000E+000

    FileInput = readLines(infile) 
    skip_line <- grep("^Events$",FileInput)

    table <- read.csv(infile, skip=skip_line, sep="\t")
    
    states <- table[,5]
    times  <- table[,2]

    data  <- data.frame(states, times)
    
    return(data)
}









relative_time <- function(table) {

    states <- table$states
    times  <- table$times

    
    times  <- diff(times)
    
    # remove the first pulse, and ignore the trailing end-state
    states <- states[1:length(states)-1]
    
    data  <- data.frame(states, times)
    return(data)
}










correct_risetime <- function(Tr, table) {


    ### Tr = rise time (in µs!!)
    ### 
    ### Examples:
    ### 
    ###     14.72596465   (CDC match)
    ###     14.77155587   (Narges match)
    ###     14.748725     (HighRes Paper match)
    ###     0.484633      (SIM no filter match)
    ###     4.92258194    (PC best match)
    ###     35.0052278    (Old match)

    # Tr <- 14.77155587 

    
    Trm = Tr / 1000           ### ASK ABOUT THIS CONSTANT!!! IT'S DIFFERENT IN THE TWO FILES
    a1  = 0.5382 * Trm
    a2  = 0.837  * Trm**(-2)
    a3  = 1.120  * Trm**(-3)

    rescale <- function(T) {
        ### undo the effect of a guassian filter to one time interval
        T <- 1000 * T
	return( T + a1 * exp(- T / a1 - a2 * T**2 - a3 * T**3) )
    }

    states <- table$states
    times  <- table$times

    ### This is somewhat problematic, as the length of the whole file increases.
    ### In thi future, it's worth exploring more elegant solutions to this

    times <- rescale(times)
    
    data  <- data.frame(states, times)
    return(data)

}






correct_sigfig <- function(table) {

    states <- table$states
    times  <- table$times
    
    times <- signif(times, 4)

    data  <- data.frame(states, times)
    return(data)

}




write.dwt <- function(table, file) {

    header <- sprintf("Segment: %d   Dwells: %d\r", 1, length(table$states))

    write(header, file) 

    states <- table$states
    times  <- table$times

    times <- sprintf("%.6f", times)
    
    data  <- data.frame(states, times)
    
    write.table(data, file, append=TRUE, sep="\t", col.names=FALSE, row.names=FALSE, eol="\r\n", quote = FALSE) 
    
}




    






table <- read.evt(infile)
table <- relative_time(table)
table <- correct_risetime(Tr=14.77155587,table)
table <- correct_sigfig(table)

write.dwt(table, "new-file-test.dwt")


























plot_hist <- function (times,title) {

    ### x - log
    ### y - sqrt
    
    main=paste(title, "histogram (sqrt frequency)", sep=" ")
    xlab=paste(title, "(log10)", sep=" ")
    ylab="Frequency (sqrt)"

    
    tabulate <- hist(log10(times$time), plot = FALSE)

    breaks <- tabulate$breaks
    breaks <- breaks[-length(breaks)] # remove last break (fencepost thing)

    counts <- tabulate$counts

    plot(breaks, sqrt(counts), type='s', xlab=xlab, ylab=ylab, main=main)
    
}








open_times   <- subset(table, state == 1, select=time)
closed_times <- subset(table, state == 0, select=time)




plot_hist(open_times, "Open Times")
plot_hist(closed_times, "Closed Times")













chunk_bursts <- function(table, t_crit) {

    break_func <- function(row) {
        row$times > t_crit & row$state == 0
    }
    
    pauses <- break_func(table)
    
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
    

    chunk_selectors <- Filter(Negate(is.null), sapply(1:length(pauses), find_next))

    chunk <- function(sel) {
        data.frame(table[sel,,])
    }

    chunks <- sapply(chunk_selectors, chunk)
    
    return(chunks)

}


chunks <- chunk_bursts(table, 0.1)


write_chunks_to_file <- function (chunks) {

    len <- floor(log10(length(chunks)))
    str <- sprintf("bursts/%%s/%%s-%%0%dd.dwt", len)

    print(str)
    
    time <- format(Sys.time(), "%F-%H-%M")
    dir.create("bursts")
    dir.create(file.path("bursts", time))
    for (i in 1:round(length(chunks)/2)) {
        file_name <- sprintf(str, time, file_prefix, i)
        write.dwt(chunks[,i], file_name)
    }
    
}

write_chunks_to_file(chunks)




### Now, fitting exponential models to the data ...

### https://www.researchgate.net/post/What_are_good_methods_for_fitting_a_sum_of_exponentials_to_data_without_an_initial_guess
### 




#### NOTE !!!! CHUNK THE BURSTS
