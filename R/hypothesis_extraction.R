#' Extract Hypotheses
#' The following script extracts hypotheses from pre-processed text.
#

# Library ---------------------------------------------------------------------
library(dplyr)
library(fastTextR)
library(stringr)
library(tokenizers)


# Data -------------------------------------------------------------------------
## Table for correcting incorrectly split words
path_split <-  "./../data/processing_next_line_split_error.csv"
df_word_split_error <- read.csv(file = path_split, 
                                stringsAsFactors = FALSE)

word_split_error <- df_word_split_error %>% select(word) %>% pull

## Hypothesis Classification Model
path_ft_model <- "./../models/fasttext_model.bin"
ft_model <- ft_load(path_ft_model)

# REGEX Strings ---------------------------------------------------------------

## Identify Hypo _ #:
regex_hypo_marker <- "<split>hypo (.*?):"

# Functions -------------------------------------------------------------------

# FASTTEXT MODEL CLASSIFICATION CHECK

apply_fasttext <- function(hypothesis_entity, hypothesis_causality) {
  # Verify Hypothesis Class with fastText Model
  ## Generate Hypothesis Prediction Dataframe
  
  print("Appy fastText")
  hypothesis_pred <- ft_predict(ft_model, 
                                newdata = hypothesis_entity, 
                                rval = "dense") %>% 
    as.data.frame()
  
  ## Prediction Column Names
  col_names <- names(hypothesis_pred)
  
  ## Drop any statements were predicted as non-hypothesis class. 
  
  if (!("__label__0" %in% col_names)) {
    
    response <- vector(mode = "logical", length = length(hypothesis_entity))
    
    for (i in seq_along(hypothesis_entity)){
      
      response[i] <- TRUE
    
      }
    
  } else if (!("__label__1" %in% col_names)) {
    
    response <- vector(mode = "logical", length = length(hypothesis_entity))
    
    for (i in seq_along(hypothesis_entity)){
      
      response[i] <- FALSE
    
      }
    
  } else {
    
    response <- hypothesis_pred %>% 
      mutate(
        Response = if_else(.[[1]] > .[[2]], FALSE, TRUE)
      ) %>% pull(Response)
  
  }
  
  hypothesis_causality <- hypothesis_causality[response]
  hypothesis_entity <- hypothesis_entity[response]
  
  output_hypothesis <- vector(mode = "list", length = 2)
  output_hypothesis[[1]] <- hypothesis_causality
  output_hypothesis[[2]] <- hypothesis_entity
  
  return(output_hypothesis)
}


#' Extract Hypotheses
#' The following function accepts processed text in character vector form
#' and returns hypothesis statements.
#


hypothesis_extraction <- function(input_text, fasttext_tag = TRUE){
  # Tokenize, Sentence --------------------------------------------------------

  # Concatenate All Vector Elements, Separated By Line Split
  processing_text <- str_c(input_text, collapse = " ")
  processing_text <- tokenize_sentences(processing_text,
                                        strip_punct = FALSE) %>% unlist()

  # Replace Double Spaces
  processing_text <- str_replace_all(string = processing_text,
                            pattern = "  ",
                            replacement = " ")

  # Normalize Text ------------------------------------------------------------
  processing_text <- tolower(processing_text)

  
  # Hypothesis Extraction -----------------------------------------------------
  # Identify Lines with Hypothesis Pattern
  h_match <- processing_text %>% str_match(regex_hypo_marker)
  
  # Extract Hypotheses Number
  h_match_num <- h_match[,2]
  
  # Identify Unique Hypothesis Numbers
  h_match_num_unq <- unique(h_match_num)
  
  # Drop NA
  h_match_num_unq <- h_match_num_unq[!is.na(h_match_num_unq)]
  
  # Determine Vector Index of Initial Hypothesis Statements
  h_initial <- c()
  for (i in h_match_num_unq){
    intial_idx <- tapply(seq_along(h_match_num),
                         h_match_num,
                         min)[i]
    h_initial <- c(h_initial, intial_idx)
  }
  
  # Reduce Text to Only Initial Hypothesis Instances
  h_statements <- processing_text[h_initial]
  
  
  # Split Statements On Indicator (Defined in Processing) ---------------------
  ## Define
  split_indicator <- "<split>"

  ## Split on Indicator
  h_statements <- str_split(string = h_statements,
                                     pattern = split_indicator) %>%
    unlist()

  ## Detect Statements Which Contain "Hypo"
  logical_hypothesis_2 <- str_detect(h_statements, "hypo")

  ## Drop Statements that Do Not Include "Hypo"
  h_statements <- h_statements[logical_hypothesis_2]
  
  # Drop Duplicate Hypothesis Calls
  ## Extract Hypothesis Number
  h_number <- h_statements %>% 
    str_extract("hypo (.*?):") %>% 
    str_remove_all("hypo ") %>% 
    str_remove_all(":") %>% 
    as.integer()
  
  ## Identify Duplicate Hypothesis Numbers
  logical_hypothesis_3 <- vector(mode = "logical", length = length(h_number))
  h_tracker <- vector(mode = "integer", length = length(h_number))
  
  for (i in seq_along(h_number)) {
    
    num <- h_number[i]
    
    if (is.na(num)){
      
      logical_hypothesis_3[i] = FALSE
      h_tracker[i] <- -1
      
    } else if (num %in% h_tracker) {
      
      logical_hypothesis_3[i] = FALSE
      h_tracker[i] <- -1
      
    } else {
      
      logical_hypothesis_3[i] = TRUE
      h_tracker[i] <- num
      
    }
  }
  
  ## Drop Duplicates and Non-Hypotheses
  h_statements <- h_statements[logical_hypothesis_3]
  
  # Fix Words Split over New Line
  for (i in seq_along(word_split_error)) {
    # Select Incorrect and Fixed Words
    word_split <- word_split_error[i]
    word_fix <- str_replace_all(string = word_split, 
                                pattern = " ",
                                replacement = "")
    
    # Replace All Instances
    h_statements <- h_statements %>% 
      str_replace_all(pattern = word_split, 
                      replacement = word_fix)
  }
  
  # Save Current State
  hypothesis_causality <- h_statements
  
  # Drop ~Hypo #:~
  hypothesis_entity <- gsub(".*: ","", h_statements)
  
  # Apply fasttext?
   if (fasttext_tag) {
  
    # Filter by Fasttext Model
    if (!(purrr::is_empty(hypothesis_entity))) {
      
      output_hypothesis <- apply_fasttext(hypothesis_entity, hypothesis_causality)
  
      hypothesis_causality <- output_hypothesis[[1]]
      hypothesis_entity <- output_hypothesis[[2]]
    }
     
   }
    
  # Create Dataframe with Hypothesis Number and Hypothesis
  df_hypothesis <- data.frame(hypothesis_entity,
                              hypothesis_causality,
                              stringsAsFactors = FALSE)

  # Rename and add Hypothesis Number
  df_hypothesis <- df_hypothesis %>%
    rename(hypothesis = hypothesis_entity) %>%
    mutate(
      h_id = paste0("h_", row_number())
    ) %>%
    select(h_id, hypothesis, hypothesis_causality)

  return(df_hypothesis)

}

