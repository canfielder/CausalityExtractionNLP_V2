#' The following script stores the functions related to the Causality 
#' Classification actions in the Causality Extraction NLP project.
#

# Library ---------------------------------------------------------------------
library(quanteda)
library(reticulate)
library(stringr)
library(tidyr)
library(tidytext)

# Import ----------------------------------------------------------------------
## Python Modules
np <- import("numpy")
joblib <- import("joblib")
nltk_stem <- import("nltk.stem")

## Causality Classification Model
path_model <- "./../models/causality_bow_pipeline_logistic_regression.pkl"
model_causality <- joblib$load(path_model)

## Regex
pattern_punct <- "[[:punct:]]"


#' Functions -------------------------------------------------------------------
#' 
#' CAUSALITY MODEL INPUT
#' The following function process the raw hypothesis strings into the format for
#' the Causality Class model input.
#' 
#' INPUT
#' * hypothesis
#'  Dataframe output from extracted hypothesis
#'  
#'  OUTPUT
#'  * sentence
#
gen_causality_model_input <- function(hypothesis) {
  
  # Generate Datasets ----------------------------------------------------------
  
  ## Extracted Entities
  entities <- entity_extraction(hypothesis)
  
  ## Causality Classification Input
  hypothesis_causality <- hypothesis %>% select(hypothesis_causality)
  
  # Text Processing ------------------------------------------------------------
  ##  Drop Punctuation / Replace Entities w/ Normalized Tags
  df_causality <- hypothesis_causality %>% 
    bind_cols(entities) %>% 
    mutate(
      row_id= row_number()
    ) %>% 
    select(row_id, everything()) %>% 
    drop_na()  %>% 
    mutate(
      hypothesis_causality = str_remove_all(hypothesis_causality, 
                                            pattern_punct),
      cause = str_remove_all(cause, pattern_punct),
      effect = str_remove_all(effect, pattern_punct)
    ) %>% 
    mutate(
      causal_statement = str_replace(hypothesis_causality, cause, "node1"),
      causal_statement = str_replace(causal_statement, effect, "node2")
    )
  
  ## Remove Stopwords
  df_causality <- df_causality %>% 
    unnest_tokens(word, causal_statement) %>% 
    anti_join(get_stopwords(), by = "word") %>% 
    select(row_id, word)
  
  ## Lemmatize Words
  ### Extract Words as Vector
  tokens <- df_causality %>% pull(word)
  
  ### Initialize
  tokens_lemm = vector(mode = "character", length = length(tokens))
  lemmatizer <- nltk_stem$WordNetLemmatizer()
  
  ### Execute Lemmatization
  for (i in seq_along(tokens)) {
    token = tokens[i]
    token_lemm <- lemmatizer$lemmatize(token)
    tokens_lemm[i] = token_lemm
  }
  
  ### Replace Lemmatized Words and Convert Tokens to Sentences as Vector
  model_input <- df_causality %>% 
    bind_cols(tokens_lemm) %>% 
    rename(word_lemm = "...3") %>% 
    group_by(row_id) %>% 
    mutate(sentence = str_c(word_lemm, collapse = " ")) %>%
    select(-word, -word_lemm) %>% 
    distinct() %>% 
    pull(sentence)
  
  return(model_input)
}


#' CAUSALITY CLASS PREDICTIONS
#' The following function generates Causality Class predictions
#' 
#' INPUT
#' * model_input
#'  
#'  OUTPUT
#'  * causality_pred
#
gen_causality_class <- function(model_input) {
  
  # Convert to Numpy Array
  model_input_np <- np$array(model_input)
  
  causality_pred <- model_causality$predict(model_input_np)
  
  return(causality_pred)
  
}

#' CAUSALITY CLASSIFICATION WRAPPER
#

causality_classification <- function(hypothesis) {
  
  # Process Hypothesis into Model Input
  model_input <- gen_causality_model_input(hypothesis)
  
  # Generate Causality Predictions
  causality_pred <- gen_causality_class(model_input)
  
  causality <- data.frame(causality_pred)
  
  return(causality)
}
  
