# Process data for no-read-depth rank abundance plots

preprocess_for_rank_abund <- function(d) {
  
  #browser()
  
  # Get rid of those garbage-ass columnss
  d <- select(d, Environment_KL, Locus_Tag, Domain.mg, Domain.mothur, Phylum, Class, Order, Family, Genus, full.tax, pident.against.cultures) %>%
    # Note: Domain.y is the mothur domain, Domain.x is the pident-related domain
    
    # Bin the pident values
    # mutate(pident.against.cultures, Similarity_bin = cut(pident.against.cultures, c(-Inf, 86- 1e-8, 96.6, Inf),
    #                                                      labels = c("Uncultured phyla", "Uncultured class to genus", "Cultured"))) 
    # 
    mutate(Similarity_bin = cut(pident.against.cultures, c(-Inf, 86- 1e-8, 96.6, Inf),
                                                         labels = c("Uncultured phyla", "Uncultured class to genus", "Cultured"))) 
                                                         
 # write_csv(d, "results/deleteme_d.csv")
  
  # Count the number of representatives of each genome
  d_summ <- d %>%
    group_by(Environment_KL, Domain.mg, Domain.mothur, Phylum, Class, Order, Family, Genus, full.tax, Similarity_bin) %>%
    summarise(n.seqs = n()) %>%
    group_by(Environment_KL, Domain.mothur, Phylum, Class, Order, Family, Genus, full.tax) %>%
    mutate(n.bins = n()) %>%
    ungroup()
  
  # Note: we have now elimiteated this entire 
  # # OK, so for each taxon and bin, need to identify which bin is the most populated, and assign all taxa that same bin
  # # Then re-group by taxa and environment, sum again, and use that data frame
  # #browser()
  # d_rebinned <- d_summ %>%
  #   group_by(Environment_KL, Domain.mothur, Phylum, Class, Order, Family, Genus, full.tax) %>%
  #   # arrange by decreasing 
  #   mutate(all.bins.count = sum(count)) %>%#,
  #   arrange(Environment_KL, Domain.mothur, Phylum, Class, Order, Family, Genus, full.tax, Similarity_bin, desc(count)) %>%
  #   mutate(true.bin = Similarity_bin[1]) %>%
  #          #diagnostic
  #   #       true.bin = Similarity_bin[count == max(count)]) %>%
  #   arrange(desc(n.bins), full.tax, desc(count)) %>%
  #   # at this point (above), true.bin and all.bins.count is the data we want to plot
  #   # But before moving on, it would be good to calculate what fraction of total counts were originally assigned to hte 'true' bin
  #   
  #   
  #   
  #   summarise(sim.bin = true.bin[1],
  #             sum.count = all.bins.count[1],
  #             frac.correctly.assigned = max(count / all.bins.count)) #%>%
  # #write_csv("results/deleteme_pick_true_bin_test.csv")
  # # CHeck quality statistics
  # median_bin_fraction <- median(d_rebinned$frac.correctly.assigned) # median is 1
  # mean_bin_fraction <- mean(d_rebinned$frac.correctly.assigned) # mean is 0.94
  # # probably shoudl calculate the fraction of taxa that appear in multiple bins, but I would have to do that abvoe here
  # # write_csv(d_summ, "results/Karen_please_check_are_bins_right.csv")
  
  # # Make a df of the top n genera in each environment
  # n.cutoff <- 10
  # 
  # d_all_top10 <- d_rebinned %>%
  #   group_by(Environment_KL) %>%
  #   top_n(n=n.cutoff, wt = sum.count) %>%
  #   mutate(sum.in.top.10 = sum(sum.count)) %>% # I think it makes sense to exclude some from this analysis if there aren't enough samples in analysis
  #   arrange(Environment_KL, desc(sum.count))
  # 
  # # d_summ <- ungroup(d_summ)
  # 
  # d_rebinned <- d_rebinned %>%
  #   ungroup() # the groupings seem to cause all sorts of problems down the line
  
  d_summ
  
}