### 
# Script to add all transcriptomic data
# Same as 2018_01_17, except with different input file
###

transcripts_no_read_depth <- function() {
  
  
  # raw_d_ts<- read_csv("data/transcriptomes_all_over90pct_alignedNR_or_cultures.csv")
  # raw_d_ts$Genome_ID <- as.character(raw_d_ts$Genome_ID)
  # raw_d_ts$Domain <-ifelse(grepl("^Arc_", raw_d_ts$sseqid), "Archaea", "Bacteria") # sseqid, not sseqid.x for whatever reason
  # 
  # # Fix environments
  # trans.environments <- c("Seawater" = "Seawater", "Soil" = "Soil", "Freshwater" = "Freshwater",
  #                         "Bioreactor" = "Bioreactor", "Host" = "Host-associated", "Hot spring" = "Hot spring",
  #                         "Terrestrial subsurface" = "Terrestrial subsurface")
  # raw_d_ts$Environment_KL <- trans.environments[raw_d_ts$Environment_KL]
  # 
  # # Calculate similarity bin (but using cut() rather than manual old method)
  # raw_d_ts_edit <-raw_d_ts %>%
  #   select(Domain, Environment_KL, Genome_ID, pident.against.cultures) %>%
  #   mutate(Similarity_bin = cut(pident.against.cultures, 
  #                               breaks = c(-Inf, 86-1e-8, 96.6, Inf),
  #                               labels = c("uncultured phylum", "uncultured genus to class", "cultured"),
  #                               right = TRUE,
  #                               include.lowest = TRUE)) 
  # 
  # # Merge in mothur phylogenetic assignments; remove chloroplasts and mitochondria
  # # This is a copy of code in the rank_abund file
  # mothur_ts <- read_csv("data/transcriptomes.precluster.nr_v132.wang.csv")
  # 
  # # REmove parentheses at the end of taxon names
  # mothur_ts <- mothur_ts %>%
  #   mutate(Domain = remove_end_paren(Domain), 
  #          Phylum = remove_end_paren(Phylum),
  #          Class = remove_end_paren(Class),
  #          Order = remove_end_paren(Order),
  #          Family = remove_end_paren(Family),
  #          Genus = remove_end_paren(Genus))
  # 
  # # merge with the transcriptome files
  # d <- inner_join(raw_d_ts, mothur_ts, by = "Locus_Tag") # 44243 rows, down from 54014 (mg_16S) or 47368 (m
  # 
  # # Remove anything marked as chloroplasts or mitochondria
  # d <- d[(!str_detect(d$Class, "Chloroplast")) & (!str_detect(d$Family, "Mitochondria")), ]  
  # 
  raw_d_ts_edit <- preprocess_ome_data(ome.fn = "data/transcriptomes_all_over90pct_alignedNR_or_cultures.csv",
                                       mothur.fn = "data/transcriptomes.precluster.nr_v132.wang.csv",
                                       merged.data.fn = "results/merged_metatranscriptome_16S_data.csv",
                                       save.files = save.files)
  
  # Little diversion here: calculating fraction in each bin without regard to environment
  row_by_row_coarse_bins <- raw_d_ts_edit %>% 
    group_by(Domain) %>% 
    mutate(n_rows = n()) %>%
    group_by(Domain, Similarity_bin) %>%
    summarise(frac = n() / n_rows[1])
  
  if(save.files) {
    write_csv(row_by_row_coarse_bins, "results/transcript_row_by_row_coarse_bin.csv") 
  }
  
  # Make sure all similarity bins appear in each environment, even if they are unrepresented in the data set
  uniques <- expand.grid(Domain = unique(raw_d_ts_edit$Domain), 
                         Environment_KL = unique(raw_d_ts_edit$Environment_KL),
                         # Genome_ID = unique(metagenomes_to_count$Genome_ID),
                         Similarity_bin = unique(raw_d_ts_edit$Similarity_bin), 
                         stringsAsFactors = FALSE) %>%
    arrange(Domain, Environment_KL, Similarity_bin)
  
  
  # For each domain, calculate the fraction of rows that are in that bin that is in each bin
  environments_by_bin_ts <- raw_d_ts_edit %>%
    right_join(uniques, by = c("Domain", "Environment_KL", "Similarity_bin")) %>% # n.b this adds 2 whole rows: from 54,014 to 54,016
    mutate(pident.against.cultures = ifelse(is.na(pident.against.cultures), 0, pident.against.cultures)) %>%
    select(Domain, Environment_KL, Similarity_bin) %>%# For convenience when I'm looking at the DF
    
    # Calculate the total number of rows in each Domain, environment, and similarity bin
    group_by(Domain, Environment_KL, Similarity_bin) %>%
    summarise(n_sequences = n()) %>%
    group_by(Domain, Environment_KL) %>%
    
    # Calculate the percent of sequences in each similarity bin
    mutate(PctN = n_sequences / sum(n_sequences, na.rm = TRUE)) 
  
  # Make plot of fraction of sequences in each culture bin, by environment
  p_trans <- ggplot(environments_by_bin_ts, aes(x=Environment_KL, y=PctN)) +
    geom_point() + 
    facet_grid(Similarity_bin ~ Domain) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = -45, hjust = 0)) + 
    ggtitle("All transcrips (no read depth)")
  print(p_trans)
  
  if(save.files) {
    write_csv(environments_by_bin_ts, "results/transcripts_no_read_depth.csv")
  }
 
  environments_by_bin_ts
}
# 
# result <- environments_by_bin_ts
# 
# rm(list = ls()[!(ls() %in% "result")])