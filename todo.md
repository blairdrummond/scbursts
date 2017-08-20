# to verify the files in bursts
- note that the front and back of the sequence are removed. Count the missing entries.

- DESTRUCTIVE:
- `sed -i '1d' * | cat | uniq > file.txt` 
- `cat * | grep -v Dwell | uniq > ../big`



[Number of states](https://www.researchgate.net/post/What_are_good_methods_for_fitting_a_sum_of_exponentials_to_data_without_an_initial_guess)


# Build
R CMD BUILD batch.R



# Object
http://adv-r.had.co.nz/OO-essentials.html

Instead of using a dataframe, create an object that also stores all the metadata, including:

	- Original filename   (then just convert evt -> dwt)
	- Durations



# TODO: Sort bursts by P(Open)

##############################################################################
   
		  Mixture of exponentials + Laplace transform
 
##############################################################################
  
	    Filter bursts by P(Open):
		    - Create a folder with all appropriate bursts
		    - ALSO: Write a dwt with big zeros seperating the bursts.
 
##############################################################################


Blair had progessed with evt to dwt conversion will make sure that the first
and last dwt events are "1" (i.e. openings).

Decided to truncate corrected durations at 6 digits in our corrected dwt files.

Popen plots for bursts, will try and generate some Popen/burst histograms.
 Also will filter the bursts based on their Popen and writing them to a dwt
(both 1 burst/dwt and all filter bursts/dwt)

Also talked about timestamping each burst so that we can plot a Popen vs time.

heuristic fitting of dwell time histograms.  Corrie will send Blair some
screenshots of TACfit histograms with fits.

