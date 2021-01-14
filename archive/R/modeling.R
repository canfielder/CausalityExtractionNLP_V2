#' The following script contains functions related to generating
#' models with the provided training dataset.
#

# Library ---------------------------------------------------------------------
library(caTools)
library(dplyr)
# library(quanteda)
library(reticulate)
library(stringr)
# library(textstem)
library(tidytext)

# Functions -------------------------------------------------------------------

#' Trim Strings
#' The following function trims they hypothesis strings. The trim methodology is
#' consistent with the original python package. This function identifies
#' where two terms are in each string, node1 and node2, although the function
#' is written in a general form in case these terms change. The function then
#' concatenates all tokens into a single token which occur after the first
#' node2 instance which occurs after a node1 instance. If this condition does
#' not exist, no changes are made.
#'
#

trim_strings <- function(input_string, key_1="node1", key_2="node2"){

  # Convert String into Word Tokens
  tokens <-  str_split(input_string, pattern =  " ") %>% unlist()

  # Determine All Indices of Key 1 and Key 2
  index_k1_all <- which(tokens %in% key_1)
  index_k2_all <- which(tokens %in% key_2)
  if(length(index_k1_all) == 0 | length(index_k2_all) == 0){
    print("Length Zero")
    print(index_k1_all)
    print(index_k2_all)

  }

  # Verify An Instance of Key 2 exists After Key 1
  # If Not, No Action
  if(length(index_k1_all) == 0 | length(index_k2_all) == 0){
    return(input_string)
  }
  else if (max(index_k2_all) < min(index_k1_all)){
    return(input_string)
  }

  # Reduce Key 2 Indices to First Instance After Key 1 Intial Instance
  logical_k2 <- (index_k2_all) > min(index_k1_all)
  index_k2_post_k1 <- min(index_k2_all[logical_k2])

  # Determine String Length
  num_tokens <- length(tokens)

  # Initialize
  output_string <- input_string

  # Trim String After Key 2
  if(num_tokens > index_k2_post_k1){
    # Collapse Tokens
    index_trim_start <- index_k2_post_k1 + 1
    tokens_trim <- str_c(tokens[index_trim_start:num_tokens], collapse = "")

    string_maintain <- str_c(tokens[1:index_k2_post_k1], collapse = " ")

    # Replace Tokens With Collapsed Trim
    output_vec <- c(string_maintain, tokens_trim)
    output_string <- str_c(output_vec, collapse = " ")
  }

  output_string <- unname(output_string)
  return(output_string)
}

# Vectorize Function
trim_strings <- Vectorize(trim_strings)


#' Process Data - Training
#' The following function performs all processing steps the precede
#' vectorization into a DTM for the provided training data set.
#'
#' Input (Required):
#' * Dataframe with the following columns:
#'  * sentence
#'  * node_1
#'  * node_2
#'  * file_name
#'  * hypothesis_num
#

process_data_train <- function(input_df){

  # Missing Values -----------------------------------------------------------------
  ## Drop Rows with Missing Values
  processing_df <- input_df %>%
    drop_na()

  # Normalize -----------------------------------------------------------------
  ## Normalize Sentence and Node Text
  processing_df <- processing_df %>%
    mutate(
      sentence = tolower(sentence),
      node_1 = tolower(node_1),
      node_2 = tolower(node_2)
    )

  # Entity Replacement --------------------------------------------------------
  ## Replace Patterns in Sentence Which Match Node_1 or Node_2 Columns
  ### Remove Punctuation from Node_1 and Node_2

  regex_punct <- "[.!?]"

  processing_df <- processing_df %>%
    mutate(
      node_1 = str_remove_all(node_1, pattern = regex_punct),
      node_2 = str_remove_all(node_2, pattern = regex_punct)
    )

  ## Repalce Node Entities
  processing_df <- processing_df %>%
    mutate(
      sentence = str_replace_all(sentence, pattern = node_1, replacement = "node1"),
      sentence = str_replace_all(sentence, pattern = node_2, replacement = "node2"),
    )

  # Tokenize ------------------------------------------------------------------
  ## Convert Sentence String into Word Tokens
  ## Method Removes Punctuation By Default
  processing_df <- processing_df %>%
    unnest_tokens(word, sentence)


  # Remove Stop Words ---------------------------------------------------------

  data(stop_words)

  processing_df <- processing_df %>%
    anti_join(stop_words, by = "word")


  # Lemmatize Sentence --------------------------------------------------------
  processing_df <- processing_df %>%
    mutate(
      word = lemmatize_words(word)
    )

  # Recombine Tokens into Strings ---------------------------------------------
  processing_df <- processing_df %>%
    group_by(file_name, hypothesis_num) %>%
    mutate(
      sentence = str_c(word, collapse = " ")
    ) %>%
    ungroup() %>%
    select(-word) %>%
    distinct()

  # Trim Strings --------------------------------------------------------------
  processing_df <- processing_df %>%
    mutate(
      sentence = trim_strings(sentence)
    )

  # Generate Unique Identifier ------------------------------------------------
  # Create Unique ID  ---------------------------------------------------------
  processing_df <- processing_df %>%
    mutate(
      hyp_id = str_c(file_name, hypothesis_num, row_number(), sep = "_")
    )

  return(processing_df)

}

#' Process Data - general
#' The following function performs all processing steps the precede
#' vectorization into a DTM for future predictive inputs.
#'
#' Input (Required):
#' * Dataframe with the following columns:
#'  * sentence
#'  * node_1
#'  * node_2
#'  * file_name
#'  * hypothesis_num
#
process_data_general <- function(input_df){

  # Missing Values -----------------------------------------------------------------
  ## Drop Rows with Missing Values
  processing_df <- input_df %>%
    drop_na()

  # Normalize -----------------------------------------------------------------
  ## Normalize Sentence and Node Text
  processing_df <- processing_df %>%
    mutate(
      sentence = tolower(hypothesis)
    )

  # Tokenize ------------------------------------------------------------------
  ## Convert Sentence String into Word Tokens
  ## Method Removes Punctuation By Default
  processing_df <- processing_df %>%
    unnest_tokens(word, hypothesis)


  # Remove Stop Words ---------------------------------------------------------
  data(stop_words)

  processing_df <- processing_df %>%
    anti_join(stop_words, by = "word")


  # Lemmatize Sentence --------------------------------------------------------
  processing_df <- processing_df %>%
    mutate(
      word = lemmatize_words(word)
    )

  # Recombine Tokens into Strings ---------------------------------------------
  processing_df <- processing_df %>%
    group_by(h_id) %>%
    mutate(
      sentence = str_c(word, collapse = " ")
    ) %>%
    ungroup() %>%
    rename(hypothesis = sentence) %>%
    select(-word) %>%
    distinct()

  # Trim Strings --------------------------------------------------------------
  processing_df <- processing_df %>%
    mutate(
      sentence = trim_strings(hypothesis)
    )

  # Drop Sentence
  processing_df <- processing_df %>% select(-sentence)

  return(processing_df)

}


#' Generate Document Term Matrix - Bag of Words - NGrams = 3
#' Creates a Document Term Matrix. Uses a Bag-of-Words approach
#' with an n-gram size of 3
#'
#' Input (Required):
#' * Dataframe with the following columns:
#'  * sentence
#'  * node_1
#'  * node_2
#'  * file_name
#'  * hypothesis_num
#

gen_dtm_bow <- function(input_df){

  # Create Corpus -------------------------------------------------------------
  corpus_hypo <- corpus(
    x = input_df,
    text_field = "hypothesis",
    docid_field = "h_id"
  )

  # Generate Tokens------------------------------------------------------------
  tokens_hyp <- quanteda::tokens(corpus_hypo)

  ## Ngram = 3
  tokens_hyp_ngram <- tokens_ngrams(tokens_hyp, n = 3:3)

  # Document Term Matrix ------------------------------------------------------
  dtm_hyp <- dfm(tokens_hyp_ngram)

  ## Convert to Dataframe
  dtm_hyp <- convert(x = dtm_hyp, to = "data.frame") %>%
    rename(h_id = doc_id)

  return(dtm_hyp)

}


#' Train / Test Split
#' The following function splits any provided dataset into training and test
#' sets.
#'
#' Inputs, Required:
#' * Data:
#'   This input will be a dataframe containing at a minimum, the text data
#' * Column, Text: Column name of the raw text
#'
#' * Inputs, Optional:
#' * train_split_ratio: Percent of data split into training set
#'


split_train_test <- function(input_data,
                             target_col = causal_relationship,
                             train_split_ratio = 0.75){

  # Enquote Column Names -------------------------------------------------------

  target_col <- enquo(target_col)

  # Process --------------------------------------------------------------------
  ## Create Temporary Column For Train/Test Split
  input_data_split <- input_data %>%
    mutate(
      splitting_col = row_number()
    )

  ## Split into Training/Test Set
  sample <- sample.split(input_data_split$splitting_col,
                         SplitRatio = train_split_ratio)

  train <- subset(input_data, sample == TRUE)
  test <- subset(input_data, sample == FALSE)

  # Extract Target and Features
  train_target <- train %>% select({{ target_col }})
  train_features <- train %>% select(-{{ target_col }})
  test_target <- test %>% select({{ target_col }})
  test_features <- test %>% select(-{{ target_col }})


  split_data <- list("train_target" = train_target,
                     "train_features" = train_features,
                     "test_target" = test_target,
                     "test_features" = test_features)

  return(split_data)

}

#' Convert Train/Test Split to Python
#' The following function converts the datasets generated by
#' **split_train_test** from R to pyton
#'


train_test_to_python <- function(split_data){

  # Convert to Python
  train_target <- r_to_py(split_data$train_target)
  train_features <- r_to_py(split_data$train_features)
  test_target <- r_to_py(split_data$test_target)
  test_features <- r_to_py(split_data$test_features)

  # Unravel Targets
  train_target <- np$ravel(train_target)
  test_target <- np$ravel(test_target)


  split_data <- list("train_target" = train_target,
                     "train_features" = train_features,
                     "test_target" = test_target,
                     "test_features" = test_features)

  return(split_data)

}

#' Bag-of-Words Transformation
#' The following function accepts processed text and performs bag-of-words
#' method transformation
#

transformation_bag_of_words <- function(input_data){

  output_bow_xfrm <- gen_dtm_bow(input_data) %>%
    left_join(input_data %>% select(causal_relationship, hyp_id),
              by = c("doc_id" = "hyp_id")) %>%
    select(causal_relationship, everything()) %>%
    select(-doc_id)

  rownames(output_bow_xfrm) <- c()

  return(output_bow_xfrm)
}

#' Bag-of-Words Transformation
#' The following function accepts processed text and performs Doc2Vec
#' method transformation
#

# transformation_doc2vec <- function(input_data){
#
#   # Generate Corpus ------------------------------------------------------------
#   # Extract Text as List
#   text <- as.character(input_data$sentence)
#
#   # Define Tagged Document
#   TaggedDocument <- gensim$models$doc2vec$TaggedDocument
#
#   # Tokenize Text
#   #$ Initialize
#   train_corpus <- list()
#   i = 1
#
#   ## Generate Tagged Document w/ Tokens
#   for (hypothesis in text) {
#     tokens <- gensim$utils$simple_preprocess(hypothesis)
#     train_corpus[[i]] <- TaggedDocument(tokens, as.character(i))
#     i = i +1
#   }
#
#   ## Convert Tagged Document Corpus to Python Object
#   train_corpus <- r_to_py(train_corpus)
#
#   # Train Model ----------------------------------------------------------------
#   # Initialize
#   model = gensim$models$doc2vec$Doc2Vec(vector_size=50, min_count=2, epochs=40)
#
#   # Build Vocabulary
#   model$build_vocab(train_corpus)
#
#   # Train
#   model$train(train_corpus, total_examples=model$corpus_count,
#               epochs=as.integer(model$epochs))
#
#   # Transform Text -------------------------------------------------------------
#   # Initialize
#   embeddings_d2v <- list()
#   i = 1
#
#   # Generate Embeddings Per Row
#   for (hypothesis in text) {
#     hypothsis_tokens <- str_split(hypothesis, pattern = " ") %>% unlist()
#     vector <- model$infer_vector(hypothsis_tokens)
#     embeddings_d2v[[i]] <- vector
#     i = i +1
#   }
#
#   # Convert Embeddings to Dataframe
#   embeddings_d2v_df <- as.data.frame(embeddings_d2v)
#
#   # Transpose Dataframe
#   embeddings_d2v_df_t <- as.data.frame(t(as.matrix(embeddings_d2v_df)))
#
#   # Drop Row Names
#   rownames(embeddings_d2v_df_t) <- c()
#
#   # Add Target Variable to Dataframe
#   output_doc2vec_xfrm <- cbind(
#     causal_relationship = input_data$causal_relationship,
#     embeddings_d2v_df_t
#     )
#
#   return(output_doc2vec_xfrm)
# }

