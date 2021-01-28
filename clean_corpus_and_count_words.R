library(dplyr)
library(tidyr)
library(filesstrings)
library(tidytext)
library(tm)

# FUNCTION: rm_stops_preserve_intra_word_dashes
# From: [give credit here (link saved in Google doc)]
# This function is a more efficient way of removing stop words while preserving intra-word dashes
#   Parameters: string - the string to remove stop words from, words - the stop word list
#   Returns: the resulting string with stop words removed (preserving intra-word dashes)
rm_stops_preserve_intra_word_dashes<-function(string,words) {
  stopifnot(is.character(string))
  stopifnot(is.character(words))
  splitted <- strsplit(string, "[ \n\t]")
  if (length(string) > 1)
    res <- vapply(splitted, function(x) paste(x[!x %in% words], collapse = " "), character(1L))
  else
    res <- paste(splitted[[1L]][(splitted[[1L]]) %in% words], collapse = " ")
  return(res)
}

# FUNCTION: prepare_corpus
# This function moves files to a separate folder so they can be read processed as
# their own corpus and saves this corpus as an R object
#   Parameters: file_set - a list of files to be in the corpus, origin_folder - 
#               a directory where files can be found, corpus_folder - a
#               working directory designated for corpora to be temporarily stored **must be an empty folder**
#           (NOTE: please write directories without end slash)
#   Returns: the corpus to be cleaned
#   Directory ending states: No changes (origin_folder still retains files, corpus_folder is still empty)
prepare_corpus<-function(file_set,origin_folder,corpus_folder){
  file.copy(unlist(lapply(file_set,function(x) paste0(origin_folder,"/",x))),corpus_folder)
  print(paste(Sys.time(),"files copies"))
  
  #create corpus to be cleaned
  mini.clean<-Corpus(DirSource(corpus_folder))
  print(paste(Sys.time(),"corpus made"))
  
  # delete file copies now that they have been saved as a corpus
  unlink(lapply(file_set,function(x) paste0(corpus_folder,"/",x)))
  print(paste(Sys.time(),"files deleted"))
  
  return(mini.clean)
}

# FUNCTION: clean_corpus
# This function cleans a corpus, converting everything to lowercase, removing numbers,
# punctuation (excepting intra-word dashes), and all other special characters that were not
# whitespace or letter characters in Brazilian Portuguese (excepting intra-word dashes), stripping
# whitespace from words, and removing stopwords
#   Parameters: mini.clean - a corpus to be cleaned
#   Returns: the cleaned corpus
clean_corpus<-function(mini.clean){
  # lowercases letters
  mini.clean<-tm_map(mini.clean,content_transformer(tolower))
  # removes special characters and numbers (i.e. anything not a letter char in Portuguese, a new line, space, or punctuation mark)
  # \u00A0 is preserved because it represents a special kind of space that needs to be kept. unfortunately, this also preserves punctuation
  mini.clean<-tm_map(mini.clean, content_transformer(function(x) gsub("[^a-zãáàâçéêíõóôúü \n-\u00A0]|[0-9]","", x)))
  # removes punctuation
  mini.clean<-tm_map(mini.clean,content_transformer(removePunctuation),preserve_intra_word_dashes=TRUE)
  # strips whitespace
  mini.clean<-tm_map(mini.clean,content_transformer(stripWhitespace))
  print(paste(Sys.time(),"corpus cleaned (stopwords not removed yet)"))
  
  # remove stopwords, except those hyphenated with other words
  mini.clean<-tm_map(mini.clean,content_transformer(rm_stops_preserve_intra_word_dashes),stopwords("pt"))
  print(paste(Sys.time(),"stopwords removed"))
  
  return(mini.clean)
}

# (PRIMARY1) FUNCTION: clean_and_count
# This function takes a list of files and saves a cleaned corpus and word counts by document
# It can be used on its own or be called by a wrapping chunks function (recommended for RAM efficiency)
#   Parameters: file_set - a list of files to be in the corpus, origin_folder - 
#               a directory where files can be found, corpus_folder - a
#               working directory designated for corpora to be temporarily stored **must be an empty folder**,
#               cleaned_corpus_folder - a directory to which the cleaned corpus will
#               be saved **must not be the same folder as origin_folder**, term_count_folder - a directory
#               to which word counts by document will be saved, corpus_num - a natural number to identify first 
#               corpus chunk **i.e. must be >=0**
#   Returns: NA
#   Directory ending states: Some changes (origin_folder still retains files, corpus_folder remains empty,
#                             cleaned_corpus_folder has cleaned versions of files in file_set saved under those same file
#                             names,term_count_folder has term counts stored in the file *mini.RData file where *=corpus_num)
clean_and_count<-function(file_set,origin_folder,corpus_folder,cleaned_corpus_folder,term_count_folder,corpus_num){
  # clean and save corpus
  mini.clean<-prepare_corpus(file_set,origin_folder,corpus_folder)
  mini.clean<-clean_corpus(mini.clean)
  writeCorpus(mini.clean,cleaned_corpus_folder)
  print(paste(Sys.time(),"clean corpus written"))
  
  # count terms
  mini.m<-DocumentTermMatrix(mini.clean, control=list(wordLengths=c(1,Inf), stopwords=T))
  print(paste(Sys.time(),"dtm made"))
  # remove cleaned corpus to save working memory
  rm(mini.clean)
  # convert to a more memory efficient table
  mini.m<-tidy(mini.m)
  print(paste(Sys.time(),"tidy made"))
  save(list = ls(all.names = TRUE), file = paste0(term_count_folder,"/",corpus_num,"mini.RData"))
  print(paste(Sys.time(),"tidy saved"))
  rm(mini.m)
}

# (PRIMARY1) FUNCTION: clean_and_count_in_chunks
# This function takes a list of files and saves a cleaned corpus and word counts by document,
# working in chunks for better RAM efficiency
#   Parameters: all_files - a list of files to be in the corpus, origin_folder - 
#               a directory where files can be found, corpus_folder - a
#               working directory designated for corpora to be temporarily stored **must be an empty folder**,
#               cleaned_corpus_folder - a directory to which the cleaned corpus will
#               be saved **must not be the same folder as origin_folder**, term_count_folder - a directory
#               to which word counts by document will be saved, corpus_num - a natural number to identify first 
#               corpus chunk **i.e. must be >=0**
#           (NOTE: please write directories without end slash)
#   Returns: NA
#   Directory ending states: Some changes (origin_folder still retains files, corpus_folder remains empty,
#                             cleaned_corpus_folder has cleaned versions of files in all_files saved under those same file
#                             names,term_count_folder has term counts stored in files labeled *mini.RData file where * refers
#                             to different corpus chunks)
clean_and_count_in_chunks<-function(all_files,origin_folder,corpus_folder,cleaned_corpus_folder,term_count_folder,corpus_num){
  #Chunk files_to_read into appropriately sized chunks (approximately 15000 files seems to be most manageable)
  #all_files is a list of file lists.
  all_files<-split(all_files, ceiling(seq_along(all_files)/15000))
  corpus_num = corpus_num - 1
  for (file_set in all_files) {
    corpus_num = corpus_num + 1
    print(paste(Sys.time(),corpus_num))
    clean_and_count(file_set,origin_folder,corpus_folder,cleaned_corpus_folder,term_count_folder,corpus_num)
  }
}

