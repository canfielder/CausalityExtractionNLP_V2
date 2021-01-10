#' Extract Hypotheses
#' The following script extracts hypotheses from pre-processed text.
#

# Library ---------------------------------------------------------------------
if (!require(pacman)) {install.packages('pacman')}
p_load(
  dplyr,
  stringr,
  tokenizers
)

# Data -------------------------------------------------------------------------
df_word_split_error <- read.csv(file ="./../data/next_line_split_error.csv")

word_split_error <- df_word_split_error %>% select(word) %>% pull

# REGEX Strings ---------------------------------------------------------------

## Identify Hypo _ #:
regex_hypo_marker <- "<split>hypo (.*?):"

# Functions -------------------------------------------------------------------

#' Extract Hypotheses
#' The following function accepts processed text in character vector form
#' and returns hypothesis statements.
#

extract_hypothesis <- function(input_text){
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
      tracker[i] <- -1
      
    } else if (num %in% tracker) {
      
      logical_hypothesis_3[i] = FALSE
      tracker[i] <- -1
      
    } else {
      
      logical_hypothesis_3[i] = TRUE
      tracker[i] <- num
      
    }
  }
  
  ## Drop Duplicates and Non-Hypotheses
  h_statements <- h_statements[logical_hypothesis_3]
  
  
  # Drop ~Hypo #:~
  h_statements <- gsub(".*: ","",h_statements)
  
  # Drop Empty Strings
  h_statements <- h_statements[h_statements != ""]
  
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
  
  # Convert to Sentence Case
  h_statements <- str_to_sentence(h_statements, locale = "en")
  
  # Create Dataframe with Hypothesis Number and Hypothesis
  df_hypothesis <- as.data.frame(h_statements,
                                 stringsAsFactors = FALSE)

  # Rename and add Hypothesis Number
  df_hypothesis <- df_hypothesis %>%
    rename(hypothesis = h_statements) %>%
    mutate(
      h_id = paste0("h_", row_number())
    ) %>%
    select(h_id, hypothesis)

  return(df_hypothesis)

}

