# Make rank order plots
rank_order_plot <- function(x, linesize, plot.type = "inset") { # I never use inset, only "main" as plot.type
  #browser()
  p <- ggplot(x) +
    geom_segment(aes(x=rank, xend=rank, y=0, yend=n.seqs, colour = Similarity_bin), size = linesize) +
   # geom_segment(aes(x=rank, xend=rank, y=0, yend=n.seqs), size = linesize) +
    scale_colour_manual(name = NULL, values = c("#01958d","#da29a8","#9f9100")) + #iwanthue from July 24 w/ dark yellow
    #scale_colour_manual(values = c('#1b9e77','#d95f02','#7570b3'))
    #scale_colour_manual(values = c('#7fc97f','#beaed4','#fdc086')) +
    #scale_colour_manual(name = NULL, values = c("#e64e30","#524ad3","#e3d12c")) + # from I want hue - I think this is what we submitted originally
    #scale_colour_manual(name = NULL, values = c("#2ca25f", "#99d8c9", "#e5f5e9")) + # This is colorbrewer BuGn (sequential)
    #scale_colour_manual(name = NULL, values = c('#a6cee3','#1f78b4','#b2df8a')) + #
    #scale_x_reverse() +
    scale_y_log10(name = "Count") +
    #scale_colour_manual(name = NULL, values = c('#e41a1c','#377eb8','#4daf4a')) +
    #scale_color_brewer(palette = "Dark2") +
    theme_bw() +
    theme(text = element_text(size = 7),
          axis.text.x = element_text(angle = -45, hjust = 0),
          axis.title.x = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank() #,
          #panel.background = element_rect(fill="gray50")
          )

  if(plot.type == "inset") {
    #browser()
    p <- p +
      #geom_point(aes(x = rank, y = n.seqs, colour = Similarity_bin)) +
      geom_point(aes(x = rank, y = n.seqs)) +
      # scale_x_discrete(labels = x$Genus[x$ordered.fulltax]) +
      scale_x_continuous(labels = x$Genus[x$rank]) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "white", colour = NA),
            plot.background = element_rect(fill = "transparent", colour = NA))
  } else {
    p <- p + theme(axis.text.x = element_blank(),
                   axis.ticks.x = element_blank())#,
                   #legend.position = "bottom")
  }

  # Boxplot method
  # What if I try geom_polygon?
  # max.rank = max(x$rank, na.rm=TRUE)
  # p <- ggplot(x) + 
  #   geom_bar(aes(x=rank, y=n.seqs, fill = Similarity_bin), 
  #            #size = linesize,
  #            stat = "identity") + 
  #   #scale_colour_manual(name = NULL, values = c("#01958d","#da29a8","#9f9100")) + #iwanthue from July 24 w/ dark yellow
  #   scale_fill_manual(name = NULL, values = c("#01958d","#da29a8","#9f9100"))  +
  #   scale_y_log10(name = "Count") + 
  #   #scale_colour_manual(name = NULL, values = c('#e41a1c','#377eb8','#4daf4a')) +
  #   #scale_color_brewer(palette = "Dark2") + 
  #   theme_bw() + 
  #   theme(text = element_text(size = 7),
  #         axis.text.x = element_text(angle = -45, hjust = 0),
  #         axis.title.x = element_blank(),
  #         panel.grid.major = element_blank(),
  #         panel.grid.minor = element_blank() #,
  #         #panel.background = element_rect(fill="gray50")
  #   )
  
  p
}
