# Overview of plots for MDM manuscript

So far I have made 5 (?) scripts to generate plots for this "microbial dark matter" manuscript. As of 1-19-2018, they are as follows:

* `all_metagenomes_w_read_depth_0-5_percent_bins.R`. This makes the 2-facet histogram-like plot of all metagenome data, weighted by read depth, which have been binned into 60-ish 0.5 percentage point bins
* `all_metagenomes_w_read_depth_coarse_bins.R`. The same as above, but bins into cultured-semicultured-uncultured bins. Writes `results/all_metagenomes_w_read_depth_coarse_bins.csv` (Does not make a plot). 
* `culturedness_by_individual_metagenome.R`. This calculates the fraction of 16S reads in each of 3 culture-classes (cultured, uncultured genus-class, uncultured phylum) for each metagenome in the data set, and then plots them each as infidivudal metagenomes and as boxplots by environment.
* `metagenomes_no_read_depth.R` which ignores sequncing depth and calculates the fraction of each environment that is in each culture bin (cultured, uncultured genus to class, uncultured phylum)
* `transcripts_no_read_depth.R`, same as above but for transcripts
* `amplicons_no_read_depth.R`, same as above but for amplicons.
    - Reads
    - Creates `results/amplicon_row_by_row_coarse_bin.csv` as side effect (% of all in each bin)
* `compile_bulk_analyses.R`, which compile the above three scripts into a single plot. Outputs `plots/similarity_by_method.png`

## To Do

* add to this file the names of the input and output files for each script
* Double check that each data type (met, amp, trans) has no more than 13 individual data types, which is the number that I get when I `bind_row()` `amp`, `trans` and `met` in `compile_bulk_analyses.R`