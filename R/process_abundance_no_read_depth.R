# Second step of processing for 'bulk' (i.e. no read depth) analysis
# This is broken out seperately because preprocess_ome_data.R is used here as well as in the read-depth dependant data

process_abundance_no_read_depth <- function(d) {
  #browser()
  # Calculate similarity bin (but using cut() rather than manual old method)
  raw_d_edit <-d %>%
    select(Domain.mg, Environment_KL, Genome_ID, pident.against.cultures) %>%
    mutate(Similarity_bin = cut(pident.against.cultures, 
                                breaks = c(-Inf, 86-1e-8, 96.6, Inf),
                                labels = c("uncultured phylum", "uncultured genus to class", "cultured"),
                                right = TRUE,
                                include.lowest = TRUE)) 
  
  # Little diversion here: calculating fraction in each bin without regard to environment
  row_by_row_coarse_bins <- raw_d_edit %>% group_by(Domain.mg) %>% 
    mutate(n_rows = n()) %>%
    group_by(Domain.mg, Similarity_bin) %>%
    summarise(frac = n() / n_rows[1])
  
  if(save.files) {
    write_csv(row_by_row_coarse_bins, "results/metagenome_row_by_row_coarse_bin.csv")
  }
  
  # Make sure all similarity bins appear in each environment, even if they are unrepresented in the data set
  uniques <- expand.grid(Domain.mg = unique(raw_d_edit$Domain.mg), 
                         Environment_KL = unique(raw_d_edit$Environment_KL),
                         # Genome_ID = unique(metagenomes_to_count$Genome_ID),
                         Similarity_bin = unique(raw_d_edit$Similarity_bin), 
                         stringsAsFactors = FALSE) %>%
    arrange(Domain.mg, Environment_KL, Similarity_bin)
  
  
  # For each domain, calculate the fraction of rows that are in that bin that is in each bin
  environments_by_bin <- raw_d_edit %>%
    right_join(uniques, by = c("Domain.mg", "Environment_KL", "Similarity_bin")) %>% # n.b this adds 2 whole rows: from 54,014 to 54,016
    mutate(pident.against.cultures = ifelse(is.na(pident.against.cultures), 0, pident.against.cultures)) %>%
    select(Domain.mg, Environment_KL, Similarity_bin) %>%# For convenience when I'm looking at the DF
    
    # Calculate the total number of rows in each Domain, environment, and similarity bin
    group_by(Domain.mg, Environment_KL, Similarity_bin) %>%
    summarise(n_sequences = n()) %>%
    group_by(Domain.mg, Environment_KL) %>%
    
    # Calculate the percent of sequences in each similarity bin
    mutate(PctN = n_sequences / sum(n_sequences, na.rm = TRUE)) 
  
  
  # # Reorder the factors so that they appear the way I want them to
  # # environments_by_bin$Similarity_bin <- factor(environments_by_bin$Similarity_bin, levels = c("uncultured phylum", "uncultured genus to phylum", "cultured species"), ordered=TRUE)
  # 
  # # THis is bac.order from 2018_01_17_metagenome_identity
  # envt.order <- c("Human-adjacent", "Human", "Hypersaline", "Hot spring", "Soil", "Bioreactor", 
  #                 "Host-associated", "Seawater", "Terrestrial subsurface", "Freshwater", 
  #                 "Marine sediment", "Snow", "Hydrothermal vent")
  # 
  # environments_by_bin$Environment_KL <- factor(environments_by_bin$Environment_KL, levels = envt.order, ordered = TRUE)
  # 
  # p_bulk_analyses <- ggplot(environments_by_bin, aes(x=Environment_KL, y=PctN)) +
  #   geom_point() + 
  #   facet_grid(Similarity_bin ~ Domain.mg) + 
  #   theme_bw() + 
  #   theme(axis.text.x = element_text(angle = -45, hjust = 0)) + 
  #   ggtitle("All metagenomes (no read depth)")
  # print(p_bulk_analyses)
  
  environments_by_bin
}