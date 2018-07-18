### 
# Script to add all transcriptomic data
# Same as 2018_01_17, except with different input file
###

amplicons_no_read_depth <- function() {
  
  raw_d_amp<- read_csv("data/amplicons.csv", col_types = "ccdcc")
  # We don't need Genome_ID
  # Domain is directly encoded so we don't need to grep to figure it out
  
  amp.envts <- c("bioreactor" = "Bioreactor", "freshwater" = "Freshwater", "Host/built" = "Host-associated",
                 "Hot spring" = "Hot spring", "Hydrothermal Vent" = "Hydrothermal vent", "Hypersaline" = "Hypersaline",
                 "Marine sediment" = "Marine sediment", "seawater" = "Seawater", "soil" = "Soil", 
                 "terrestrial subsurface" = "Terrestrial subsurface", "host" = "Host-associated", 
                 "human" = "Human", "human-adjacent" = "Human-adjacent", "Hydrothermal vent" = "Hydrothermal vent",
                 "marine sediment" = "Marine sediment", "rock" = "Rock", "snow-rain-glaciers" = "Snow")
  raw_d_amp$Environment_KL <- amp.envts[raw_d_amp$Environment_KL]
  
  # Calculate similarity bin (but using cut() rather than manual old method)
  raw_d_amp_edit <-raw_d_amp %>%
    select(Domain, Environment_KL, pident.against.cultures) %>%
    mutate(Similarity_bin = cut(pident.against.cultures, 
                                breaks = c(-Inf, 86-1e-8, 96.6, Inf),
                                labels = c("uncultured phylum", "uncultured genus to class", "cultured"),
                                right = TRUE,
                                include.lowest = TRUE)) 
  
  # # Check that bins are assigned correctly - uncultured genus to class should include both 86 exactly and 96.6 exactly
  # sum(raw_d_edit %>% 
  #   filter(pident.against.cultures == 86 | pident.against.cultures == 96.6) %>%
  #   select(Similarity_bin) == "uncultured genus to class") ==
  #   raw_d_edit %>%
  #   filter(pident.against.cultures == 86 | pident.against.cultures == 96.6) %>%
  #   nrow() # I think I have found the worst possible use case of piping
  
  # Little diversion here: calculating fraction in each bin without regard to environment
  row_by_row_coarse_bins <- raw_d_amp_edit %>% group_by(Domain) %>% 
    mutate(n_rows = n()) %>%
    group_by(Domain, Similarity_bin) %>%
    summarise(frac = n() / n_rows[1])
  # write_csv(row_by_row_coarse_bins, "results/amplicon_row_by_row_coarse_bin.csv")
  
  
  # Make sure all similarity bins appear in each environment, even if they are unrepresented in the data set
  uniques <- expand.grid(Domain = unique(raw_d_amp_edit$Domain), 
                         Environment_KL = unique(raw_d_amp_edit$Environment_KL),
                         # Genome_ID = unique(metagenomes_to_count$Genome_ID),
                         Similarity_bin = unique(raw_d_amp_edit$Similarity_bin), 
                         stringsAsFactors = FALSE) %>%
    arrange(Domain, Environment_KL, Similarity_bin)
  
  
  # For each domain, calculate the fraction of rows that are in that bin that is in each bin
  environments_by_bin_amp <- raw_d_amp_edit %>%
    arrange(pident.against.cultures) %>% # because the top is dominated by 100% cultures from humans, so head() is not informative
    #right_join(uniques, by = c("Domain", "Environment_KL", "Similarity_bin")) %>% # n.b this adds 2 whole rows: from 54,014 to 54,016
    mutate(pident.against.cultures = ifelse(is.na(pident.against.cultures), 0, pident.against.cultures)) %>%
    select(Domain, Environment_KL, Similarity_bin) %>%# For convenience when I'm looking at the DF
    
    # Calculate the total number of rows in each Domain, environment, and similarity bin
    group_by(Domain, Environment_KL, Similarity_bin) %>%
    summarise(n_sequences = n()) %>%
    group_by(Domain, Environment_KL) %>%
    
    # Calculate the percent of sequences in each similarity bin
    mutate(PctN = n_sequences / sum(n_sequences, na.rm = TRUE)) 
  
  
  # Reorder the factors so that they appear the way I want them to
  # environments_by_bin$Similarity_bin <- factor(environments_by_bin$Similarity_bin, levels = c("uncultured phylum", "uncultured genus to phylum", "cultured species"), ordered=TRUE)
  
  # # THis is bac.order from 2018_01_17_metagenome_identity
  # envt.order <- c("Human-adjacent", "human", "Hypersaline", "Hot spring", "Soil", "Bioreactor", 
  #                 "Host-associated", "Seawater", "Terrestrial subsurface", "Freshwater", 
  #                 "Marine sediment", "Snow", "Hydrothermal vent")
  # 
  # environments_by_bin_amp$Environment_KL <- factor(environments_by_bin_amp$Environment_KL, levels = envt.order, ordered = TRUE)
  p_amp <- ggplot(environments_by_bin_amp, aes(x=Environment_KL, y=PctN)) +
    geom_point() + 
    facet_grid(Similarity_bin ~ Domain) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = -45, hjust = 0)) + 
    ggtitle("All amplicons (no read depth)")
  print(p_amp)
  
  environments_by_bin_amp
  
}
# write.csv(environments_by_bin_amp, "results/amplicons_no_read_depth.csv")
# amp <- environments_by_bin_amp
# 
# rm(list = ls()[!(ls() %in% "amp")])

