# Want to do the same thing as in 2018_01_17_metagenome_identity, 
# BUT WITHOUT SUMMING 

metagenomes_no_read_depth <- function() {
  
  # raw_d <- read_csv("data/All_metagenomes_over90_length_to_either_culture_or_NR_sizes.csv")
  # 
  # raw_d$Genome_ID <- as.character(raw_d$Genome_ID)
  # raw_d$Domain <-ifelse(grepl("^Arc_", raw_d$sseqid.x), "Archaea", "Bacteria")
  # 
  # # Tweak environment categories (although I think it doesn't matter here?)
  # raw_d$Environment_KL[raw_d$Environment_KL == "human"] <- "Human"
  raw_d <- preprocess_ome_data(ome.fn = "data/All_metagenomes_over90_length_to_either_culture_or_NR_sizes.csv",
                               mothur.fn = "data/transcriptomes.precluster.nr_v132.wang.csv",
                               merged.data.fn = "data/deleteme.csv", # because this is already saved by the first metagenome preprocess step
                               save.files = save.files)
  
  metagenomes_processed <- process_abundance_no_read_depth(raw_d)
  
  # # Calculate similarity bin (but using cut() rather than manual old method)
  # raw_d_edit <-raw_d %>%
  #   select(Domain, Environment_KL, Genome_ID, pident.against.cultures) %>%
  #   mutate(Similarity_bin = cut(pident.against.cultures, 
  #                               breaks = c(-Inf, 86-1e-8, 96.6, Inf),
  #                               labels = c("uncultured phylum", "uncultured genus to class", "cultured"),
  #                               right = TRUE,
  #                               include.lowest = TRUE)) 
  # 
  # # Little diversion here: calculating fraction in each bin without regard to environment
  # row_by_row_coarse_bins <- raw_d_edit %>% group_by(Domain) %>% 
  #   mutate(n_rows = n()) %>%
  #   group_by(Domain, Similarity_bin) %>%
  #   summarise(frac = n() / n_rows[1])
  # 
  # if(save.files) {
  #   write_csv(row_by_row_coarse_bins, "results/metagenome_row_by_row_coarse_bin.csv")
  # }
  # 
  # # Make sure all similarity bins appear in each environment, even if they are unrepresented in the data set
  # uniques <- expand.grid(Domain = unique(raw_d_edit$Domain), 
  #                        Environment_KL = unique(raw_d_edit$Environment_KL),
  #                        # Genome_ID = unique(metagenomes_to_count$Genome_ID),
  #                        Similarity_bin = unique(raw_d_edit$Similarity_bin), 
  #                        stringsAsFactors = FALSE) %>%
  #   arrange(Domain, Environment_KL, Similarity_bin)
  # 
  # 
  # # For each domain, calculate the fraction of rows that are in that bin that is in each bin
  # environments_by_bin <- raw_d_edit %>%
  #   right_join(uniques, by = c("Domain", "Environment_KL", "Similarity_bin")) %>% # n.b this adds 2 whole rows: from 54,014 to 54,016
  #   mutate(pident.against.cultures = ifelse(is.na(pident.against.cultures), 0, pident.against.cultures)) %>%
  #   select(Domain, Environment_KL, Similarity_bin) %>%# For convenience when I'm looking at the DF
  #   
  #   # Calculate the total number of rows in each Domain, environment, and similarity bin
  #   group_by(Domain, Environment_KL, Similarity_bin) %>%
  #   summarise(n_sequences = n()) %>%
  #   group_by(Domain, Environment_KL) %>%
  #   
  #   # Calculate the percent of sequences in each similarity bin
  #   mutate(PctN = n_sequences / sum(n_sequences, na.rm = TRUE)) 
  # 
  # 
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
  #   facet_grid(Similarity_bin ~ Domain) + 
  #   theme_bw() + 
  #   theme(axis.text.x = element_text(angle = -45, hjust = 0)) + 
  #   ggtitle("All metagenomes (no read depth)")
  # print(p_bulk_analyses)
  # 
  # environments_by_bin
  # ggsave("plots/all_metagenomes_dotplot.png", height=4, width=6, units = "in", dpi = 300)
  # write_csv(environments_by_bin, "results/metagenomes_no_read_depth.csv")
  raw_d
}

# result <- environments_by_bin
# 
# rm(list = ls()[!(ls() %in% "result")])