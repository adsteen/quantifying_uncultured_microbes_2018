# Prepross either metagenome or metatranscriptome data
# Read file of meta-gene/transcript-ome; merge in file of mothur assignments, remove chloroplasts and mitochondria
# Create file of full output

preprocess_ome_data <- function(ome.fn, mothur.fn.primary, mothur.fn.secondary, ome.type = "unspecifed", save.files) {
  
  #Count number of sequences within each environment that have a given range of percent identities
  
  #####
  # Read sequence data, parse Domain assignent
  #####
  
  ome_16S <- suppressMessages(read_csv(ome.fn)) %>%
    mutate(Domain.mg = ifelse(grepl("^Arc_", sseqid), "Archaea", "Bacteria"),
           Genome_ID = as.character(Genome_ID))
  
  
  #######
  # Read TWO mothur outputs
  #######
  
  # Primary mothur output - first pass on taxonomic assignments
  first_pass_tax <- read_csv(mothur.fn.primary) %>%
    select(-full_taxonomy) %>%
    rename(Domain.mothur = Domain)
  
  # Secondary mothur output - run with no preclustering
  Locus_Tag <- read_delim(mothur.fn.secondary, delim = "\t", col_names = c("Locus_Tag", "deleteme"))[ , 1]
  second_pass_tax <- read_delim(mothur.fn.secondary, delim = ";", col_names = c("Domain.mothur", "Phylum", "Class", "Order", "Family", "Genus")) %>%
    bind_cols(Locus_Tag) %>%
    mutate(Domain.mothur = str_extract(Domain.mothur, "(?<=\t).+"))
  
  
  tax <- bind_rows(first_pass_tax, second_pass_tax) %>%
    mutate(Domain.mothur = remove_end_paren(Domain.mothur), 
           Phylum = remove_end_paren(Phylum),
           Class = remove_end_paren(Class),
           Order = remove_end_paren(Order),
           Family = remove_end_paren(Family),
           Genus = remove_end_paren(Genus),
           full.tax = paste(Domain.mothur, Phylum, Class, Order, Family, Genus, sep = ";")) %>%
    filter(!is.na(Locus_Tag)) %>%# THere was exactly 1, an Acidobacteria (Blastocatellaceae_(Subgroup 4) RB41), apparently a copy/paste error
    filter(!is.na(Domain.mothur)) # There were 3 unclassified sequences in mothur
  
  ome_16S_merged <- ome_16S %>%
    left_join(tax, by = "Locus_Tag") %>%
    mutate(full.tax = paste0(Domain.mg, full.tax)) %>% 
    
    # Remove chloroplasts, mitochondria, and Eukaryotes
    filter(!str_detect(full.tax, "(C|c)hloroplast")) %>%
    filter(!str_detect(full.tax, "(M|m)itochondria")) %>%
    filter(!str_detect(full.tax, "(E|e)ukaryot"))
  
  
  #Capitalize each environment name 
  if("human" %in% unique(ome_16S_merged$Environment_KL)) {
    ome_16S_merged$Environment_KL[ome_16S_merged$Environment_KL == "human"] <- "Human" 
  }
  
  # Write file of merged raw-ish data
  if(save.files) {
    write_csv(ome_16S_merged, paste0("results/all_merged_", ome.type, ".csv"))
  }
  
  # Return the merged file
  ome_16S_merged
}
