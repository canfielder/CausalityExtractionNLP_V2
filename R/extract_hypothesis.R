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

# REGEX Strings ---------------------------------------------------------------
## Identify Letters
regex_hypothesis <- "(hypo)|(h\\d+)"


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

  # Identify Hypothesis Statements --------------------------------------------
  ## Return Logical Vector
  logical_hypothesis_1 <- str_detect(processing_text, regex_hypothesis)

  ## Reduce Document to Only Hypothesis Statements
  hypothesis_statements <- processing_text[logical_hypothesis_1]

  # Split Statements On Indicator (Defined in Processing) ---------------------
  ## Define
  split_indicator <- "<split>"

  ## Split on Indicator
  hypothesis_statements <- str_split(string = hypothesis_statements,
                                     pattern = split_indicator) %>%
    unlist()

  ## Detect Statements Which Contain "Hypo"
  logical_hypothesis_2 <- str_detect(hypothesis_statements, "hypo")

  ## Drop Statements that Do Not Include "Hypo"
  hypothesis_statements <- hypothesis_statements[logical_hypothesis_2]

  # Create Dataframe with Hypothesis Number and Hypothesis
  df_hypothesis <- as.data.frame(hypothesis_statements,
                                 stringsAsFactors = FALSE)
  
  print(names(df_hypothesis))

  # Rename and add Hypothesis Number
  df_hypothesis <- df_hypothesis %>%
    rename(hypothesis = hypothesis_statements) %>%
    mutate(
      h_id = paste0("h_", row_number())
    ) %>%
    select(h_id, hypothesis)

  return(df_hypothesis)

}

