process_rank_order_data_v2 <- function(my_df, ome.type, envt, n.cutoff = 10, total.cutoff = 100, linesize = 0.1) {
  
 #browser()
  
  # d should be the full data set for a single environment
  envt <- unique(my_df$Environment_KL)
  if(length(envt) > 1) {
    warning(paste0("There appear to be multiple environments in this dataset:/n", envt))
    envt <- envt[1]
  }
  
  #browser()
  set.seed(0)
  my_df2 <- my_df %>%

   #mutate(Similarity_bin = as.character(Similarity_bin)) %>%
    # remove anything that is unclassified by mothur - that means it has NA for Domain.mothur
    filter(!is.na(Domain.mothur)) %>%
    
    # Count 
    group_by(Domain.mothur, full.tax, Similarity_bin) %>%
    mutate(count.combo = n()) %>%
    arrange(full.tax, Similarity_bin) %>%
    ungroup() %>%
    mutate(rank = rank(-1*(n.seqs+rnorm(length(n.seqs), mean = 0, sd = 0.000001))))
# 
#     # Order genus by rank
#     arrange(desc(n.seqs)) %>%
#     #mutate(Similarity_bin = as.character(Similarity_bin)) %>%
#     mutate(full.tax.by.bin = paste0(eval(full.tax),", ", eval(Similarity_bin)))# %>%
#   
#   # No idea why this is a problem, but I can't seem to do this in mutate without R coercing back to character vector
#   full.tax.by.bin.fct <- as_factor(my_df2$full.tax.by.bin)
#   full.tax.by.bin.ord <- fct_reorder(f = full.tax.by.bin.fct, x = my_df2$n.seqs, fun = mean)
#   my_df2$full.tax.by.bin.ord <- full.tax.by.bin.ord
#   
#     #mutate(full.tax.by.bin.fct = as.factor(full.tax.by.bin))
#     #fct_reorder(full.tax.by.bin)
#     #mutate(ordered.fulltax.by.bin = as.factor(full.tax.by.bin))
#   
#   #my_df3 <- my_df2 <-
#   #  mutate(ordered.fulltax.by.bin = factor(full.tax.by.bin, levels = full.tax.by.bin, ordered = TRUE)) # NOPE
#  #          ordered.fulltax = fct_reorder(full.tax.by.bin, x = n.seqs, fun = mean, .desc = TRUE)) 
# 
# 
  # Calculate top 10 (cutting out environmetns with less than total.cutoff representatives)
  top_n_seqs <- my_df2 %>%
    ungroup() %>%
    top_n(n = n.cutoff, wt = n.seqs) %>%
    arrange(desc(n.seqs))

  # save text files of whole environment, and top 10
  if(save.files) {
    fn.all <- paste0("results/rank_order_all_", envt, "_", ome.type, ".csv")
    write_csv(my_df2, fn.all)
    fn.top10 <- paste0("results/rank_order_all_top_10_", envt, "_", ome.type, ".csv")
    write_csv(top_n_seqs, fn.top10)
  }

  # Hey look I defined a global! This is almost certainly a bad idea
  my_df2 <<- my_df2
  # Make plots
  p_main <- rank_order_plot(my_df2, linesize = linesize, plot.type = "main")
  #browser()
  p_inset <- rank_order_plot(top_n_seqs, linesize = linesize, plot.type = "inset")

  ###
  # save plots
  ###

  # actual size, small full plots
  small.full.plot.fn <- paste0("plots/rank_abund/resubmit/all_rank_actual_size_fig_", envt, "_", ome.type, ".tiff")
  p_smallplot <- p_main +
    theme(text = element_text(size = 7),
          legend.position = "none",
          axis.title.y = element_blank())
  
  

  if(save.files) {
    ggsave(small.full.plot.fn, p_smallplot, height = 7.3 / 6, width = 7.3/6, units = "in", dpi = 900, compression = "lzw", type = "cairo")
  }
#   
#   # Return the example plot with the legend in it
  p <- p_smallplot + theme(legend.position = "right")
  p
}
