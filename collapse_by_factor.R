library(dplyr)
library(tidyr)
library(tidytext)

# word counts for every individual document (court decision) are way too large and are stored in chunks covering a smaller
# proportion of the entire corpus to avoid slow processing from loading too much data at once. 
# this script has functions that allow you to combine word counts across these separate chunks in meaningful ways
# i.e. it gives you the ability to aggregate word counts for all files by a particular attribute, condensing data to a more
# manageable size and while still allowing you the flexibility to recombine data in multiple different ways.

# This function will aggregate word counts in specified data files by a factor defined in a metadata column. i.e. the function
# will aggregate all word counts for "file_name"s of a particular value for the attribute of interest. It will save the aggregated
# word counts at the result_file_path
# metadata_file_path: this should be the file path of an R data file with an object named metadata that contains a "file_name" column
#                     and any factor you'd like to aggregate by. the file_names must correspond with file_names in data files
# data_directory: this should be a directory storing all data files with "file_name"s, terms, and word counts. the function assumes
#                 these files are uniformly named "data_directory/#data_file_base_name" where # is the set number your set_min and 
#                 set_max will bound
# data_file_base_name: this should be the base name for all data files with "file_name"s, terms, and word counts. as per above, the function assumes
#                 these files are uniformly named "data_directory/#data_file_base_name" where # is the set number your set_min and 
#                 set_max will bound
# chosen_factor: name of attribute to aggregate word counts by. should be the name of a column in your metadata object from metadata_directory
# set_min: # of set to begin with. the function will loop through all files "data_directory/#data_file_base_name" where #>=set_min and #<=set_max
# set_max: # of set to end with. the function will loop through all files "data_directory/#data_file_base_name" where #>=set_min and #<=set_max

collapse_by_factor<-function(metadata_file_path,data_directory,data_file_base_name,chosen_factor,set_min,set_max,result_file_path) {
  load(metadata_file_path)
  combined_by_factor<-tibble()
  for(x in set_min:set_max) {
    print(x)
    load(paste0(data_directory,x,data_file_base_name))
    by_factor <- metadata %>%
      rename(document=file_name) %>%
      select(document,{{chosen_factor}}) %>%
      right_join(mini.m) %>% 
      group_by(.data[[chosen_factor]],term,.add=TRUE) %>%
      summarize(count=sum(count),.groups="drop_last")
    rm(mini.m)
    gc()
    combined_by_factor<-bind_rows(combined_by_factor,by_factor)
    rm(by_factor)
    gc()
  }
  rm(metadata)
  combined_by_factor<-combined_by_factor %>%
    group_by(.data[[chosen_factor]],term,.add=TRUE) %>%
    summarize(count=sum(count),.groups="drop_last")
  save(list = ls(all.names = TRUE), file = result_file_path)
  return(combined_by_factor)
}

# This takes a data frame (combined_by_factor) with terms and word counts in a column labeled "count" for each chosen_factor
# value, and calculates the total word count for each chosen_factor value. This is useful for later calculating things like
# term frequency within a ~document~ (all text of a given chosen_factor) or inter ~document~ frequency.
# combined_by_factor: a data frame with a column named with the provided chosen_factor name, a term column, and a "count" column
#                     referring to term counts
# chosen_factor: a string referring to the name of the column whose values you want to calculate totals over
calculate_totals<-function(chosen_factor, combined_by_factor) {
  by_factor_totals <- combined_by_factor %>%
    group_by(.data[[chosen_factor]],.add=TRUE) %>%
    summarize(total=sum(count)) %>%
    mutate(!!chosen_factor:=replace_na(.data[[chosen_factor]],"NA"))
  combined_by_factor <- combined_by_factor %>%
    mutate(!!chosen_factor:=replace_na(.data[[chosen_factor]],"NA")) %>%
    left_join(by_factor_totals) 
  rm(by_factor_totals)
  return(combined_by_factor)
}

# This will calculate term frequency within a ~document~ and inter ~document~ frequency to get measure both term frequency and
# distinctiveness in different ~document~s. Here, the word ~document~ is used flexibly, where however the text data has been
# combined in the combined_by_factor data frame, ~document~ refers to each distinct chosen_factor value.
calculate_frequency_tfidf<-function(chosen_factor, combined_by_factor, contains_totals) {
  if (!contains_totals) {
    combined_by_factor<-calculate_totals(chosen_factor,combined_by_factor)
  }
  combined_by_factor <- combined_by_factor %>%
    bind_tf_idf(term, {{chosen_factor}}, count)
  return(combined_by_factor)
}

