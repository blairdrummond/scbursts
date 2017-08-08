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


library("devtools")

infile="data/real data.evt"
outfile="out.txt"

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

    table <- read.csv(infile, skip=skip_line+1, sep="\t")
    
    state <- table[,5]

    ### To calculate the difference between
    ### times need to add a 0 to the beginning
    ###
    ###    0.05027597	
    ###    0.05032199
    ###    0.05035479

    time  <- diff(append(0,table[,2]))
    data  <- data.frame(state, time)

    return(data)
}

table <- read.evt(infile)




# correct_risetime <- function(Tr, ) {
# 
#     
#     
# 
# }

# split(d, ceiling(seq_along(d)/20))








open_times   <- subset(table, state == 1, select=time)
closed_times <- subset(table, state == 0, select=time)


















### inverse error function
###
### invErf <- function(x) {
###     ### argument x must lie between -1 and 1
###     qnorm((1 + x) /2) / sqrt(2)
### }



























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




plot_hist(open_times, "Open Times")
plot_hist(closed_times, "Closed Times")


### Now, fitting exponential models to the data ...

### https://www.researchgate.net/post/What_are_good_methods_for_fitting_a_sum_of_exponentials_to_data_without_an_initial_guess
### 




#### NOTE !!!! CHUNK THE BURSTS
