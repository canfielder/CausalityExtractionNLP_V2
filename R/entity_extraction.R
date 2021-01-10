#' The following script stores the functions related to the Entity Extraction
#' actions in the Causality Extraction NLP project.
#

# Library ---------------------------------------------------------------------
library(reticulate)

# Import ----------------------------------------------------------------------
## Python Modules
np <- import("numpy")
tf <- import("tensorflow")

## Entity Extraction Model
path_model <- "./../models/entity_extraction/"
model <- tf$keras$models$load_model(path_model)

#' Functions -------------------------------------------------------------------
#' VECTOR IS EMPTY
#' The following function returns a logical value indicating if a vector is
#' empty.
#' 
#' #' INPUT
#' * input_vector:
#'     Input vector (vector)
#' 
#' OUTPUT
#'  *  Logic test output indicating if the vector is empty.
#
vector_is_empty <- function(input_vector) {
  return(length(input_vector) == 0)
}


#' GENERATE ENTITY CLASS PREDICTION
#' The following function converts the extracted hypotheses (string) into 
#' predicted entity classes using the Entity Extraction Model.
#'
#' INPUT
#' * hypothesis:
#'     The input text hypothesis (string)
#' 
#' OUTPUT
#'  * pred_classes:
#'      Predicted Entity classes (0,1,2) for each token in the input hypothesis
#

gen_entity_class <- function(hypothesis) {
  # Convert Hypothesis String to Numpy Array
  hypothesis_np <- np$array(hypothesis)
  
  # Generate Predictions
  pred_classes_array<- model$predict_classes(hypothesis_np)
  
  # Convert Predictions to Vector
  pred_classes <- as.vector(pred_classes_array)
  
  return(pred_classes)
}


#' GENERATE ENTITY CLASS INDEXES
#' The following function converts returns the index location of Node 1 and 
#' Node 2 entity classes
#'
#' INPUT
#'  * pred_classes:
#'      Predicted Entity classes (0,1,2) for each token in the input hypothesis
#'      
#' OUTPUT
#'  * index_entities:
#'      List containing two vector, one vector with the indexes of Entity Node 1
#'       and one vector with the indexes of Entity Node 2 (list)
#

gen_entity_class_index <- function(pred_classes){
  # Define Each Node's Indexes
  index_e1 <- which(pred_classes %in% 1) 
  index_e2 <- which(pred_classes %in% 2)
  
  # Store Indexes in List
  index_entities <- vector(mode = "list", length = 2)
  index_entities[[1]] = index_e1
  index_entities[[2]] = index_e2
  
  return(index_entities)
}


#' TRIM OVERLAPPING ENTITIES
#' The following function trims entity indexes if Node 1 and Node 2 entities
#' overlap.
#'
#' INPUT
#' * entity_index_input:
#'     List containing index vectors for each entity class (list)
#' 
#' OUTPUT
#' * entity_index_output:
#'     List containing index vectors for each entity class after 
#'     trimming process (list)
#

trim_overlapping_entities <- function(entity_index_input){
  # Extract Node 1 and node 2 Index
  entity_idx_1 <- entity_index_input[[1]]
  entity_idx_2 <- entity_index_input[[2]]
  
  # Define Node Index Range
  entity_range_1 <- min(entity_idx_1):max(entity_idx_1)
  entity_range_2 <- min(entity_idx_2):max(entity_idx_2)
  
  # Determine if Ranges Overlap
  range_overlap <- intersect(entity_range_1,entity_range_2 )
  
  if (length(range_overlap) == 0) {
    
    return(entity_index_input)
    
    # If Overlap Exists, Trim Longest Node Range
  } else {
    # Define Node Range Length
    span_entity_1 = length(entity_range_1)
    span_entity_2 = length(entity_range_2)
    
    # Determine Median of Node Indexes 
    # to Determine Which Direction to Move
    med_entity_1_idx <- median(entity_idx_1)
    med_entity_2_idx <- median(entity_idx_2)
    
    # First Node in Hypothesis - Node 1
    if (med_entity_1_idx < med_entity_2_idx) {
      if (span_entity_2 >= span_entity_1){
        entity_idx_2 <- entity_idx_2[-1]
        
      } else {
        entity_1_dim <- length(entity_idx_1)
        entity_idx_2 <- entity_idx_2[-entity_1_dim]
      }
      # First Node in Hypothesis - Node 2
    } else {
      if (span_entity_2 >= span_entity_1){
        entity_2_dim <- length(entity_idx_2)
        entity_idx_2 <- entity_idx_2[-entity_2_dim]
        
      } else {
        entity_idx_1 <- entity_idx_1[-1]
      }
    }
    # Store Trimmed Indexes For Output
    entity_index_input <- vector(mode = "list", length = 2)
    entity_index_input[[1]] = entity_idx_1
    entity_index_input[[2]] = entity_idx_2
    
    # Recursively Execute Function
    return(trim_overlapping_entities(entity_index_input))
  }
}


# TRIM OUTLIER ENTITY INDEXES
#' The following function trims entity indexes if Node 1 or Node 2 entities
#' overlap are outliers based on 1.5 * IQR criteria.
#'
#' INPUT
#' * entity_index_input:
#'     List containing index vectors for each entity class (list)
#' 
#' OUTPUT
#' * entity_index_output:
#'     List containing index vectors for each entity class after 
#'     trimming process (list)
#

trim_outlier_indexes <- function(entity_index_input) {
  # Initialize
  entity_index_output <- vector(mode = "list", length = 2)
  
  for (i in seq_along(entity_index_input)){
    
    # Define Index Vector
    index = entity_index_input[[i]]
    
    # Skip Process if Index Vector is Empty
    if (vector_is_empty(index)){
      entity_index_output[[i]] <- index
      next
    }
    
    # Calculate Summary Statistics
    summary <- as.vector(summary(index))
    
    # Define Outlier Parameters
    iqr.range <- summary[5] - summary[2]
    upper <- summary[5] + iqr.range * 1.5
    lower <- summary[2] - iqr.range * 1.5
    
    # Drop if Index is Outlier
    index <- index[index > lower]
    index <- index[index < upper]
    
    entity_index_output[[i]] <- index
  }
  
  return(entity_index_output)
}


# CONVERT ENTITY INDEX TO STRING
#' The following function converts the entity index vector to the entity text.
#'
#' INPUT
#' * hypothesis:
#'     The input text hypothesis (string)
#' * entity_index_input:
#'     List containing index vectors for each entity class (list)
#' 
#' OUTPUT
#' * entity_text_output:
#'     List containing the extracted entity text (list)
#

index_to_entity <- function(hypothesis, entity_index_input) {
  # Initialize
  entity_text_output <- vector(mode = "character", length = 2)
  
  # Convert Hypothesis to Tokens
  tokens_all = str_split(hypothesis, " ")[[1]]
  
  for (i in seq_along(entity_index_input)){
    
    # Define Index Vector
    index = entity_index_input[[i]]
    
    # Skip Process if Index Vector is Empty
    if (vector_is_empty(index)){
      entity_text_output[[i]] <- "<Entity Not Detected>"
      next
    }
  
    # Extract Entity Tokens
    tokens_entity <- tokens_all[
      min(entity_index_input[[i]]):
      max(entity_index_input[[i]])
    ]
    
    # Concatenate Tokens to Strings
    entity_text_output[i] <- str_c(tokens_entity, collapse = " ")
  }
  
  return(entity_text_output)
}


# ENTITY EXTRACTION WRAPPER - INDIVIDUAL
#' The following function executes the full Entity Extraction for a single 
#' hypothesis.
#'
#' INPUT
#' * hypothesis:
#'     The input text hypothesis (string)
#' 
#' OUTPUT
#' * entity_text_output:
#'     List containing the extracted entity text (list)
#

wrapper_entity_extraction_indv <- function(hypothesis) {
  # Generate Entity Class Predictions
  pred_classes <- gen_entity_class(hypothesis)

  index_entities <- gen_entity_class_index(pred_classes)
    
  # Trim Overlapping Entities
  ## Verify Both Entities Detected
  both_entity_present <- vector_is_empty(index_entities[[1]]) & 
    vector_is_empty(index_entities[[2]])
  
  ## Trim Overlap
  if (both_entity_present) {
    index_entities <- trim_overlapping_entities(index_entities)
  }
  
  # Remove Outliers
  index_entities <- trim_outlier_indexes(index_entities)
  
  ## Convert Indexes to Text
  entity_text_output <- index_to_entity(hypothesis, index_entities)

  return(entity_text_output)  
}


# ENTITY EXTRACTION WRAPPER - MULTIPLE
#' The following function executes the full Entity Extraction for multiple
#' hypotheses.
#'
#' INPUT
#' * lst_hypothesis:
#'     List of extracted hypotheses (list)
#' 
#' OUTPUT
#' * df_entity_text_output:
#'     List containing the extracted entity text of all hypotheses(dataframe)
#

wrapper_entity_extraction_mult <- function(lst_hypothesis){
  # Initialize Output List
  num_hypothesis <- length(lst_hypothesis)
  
  lst_entity_text_output <- vector(mode = "list", length = num_hypothesis)
  
  for (i in seq_along(lst_hypothesis)){
    # Extract Hypothesis Test
    hypothesis <- lst_hypothesis[[i]]
    
    # Extract Entities
    entity_text_output <- wrapper_entity_extraction_indv(hypothesis)
    
    # Store in Output List
    lst_entity_text_output[[i]] <- entity_text_output
  }
  # Convert List of Lists to Dataframe
  df_entity_text_output <- as.data.frame(
    do.call(rbind, lapply(lst_entity_text_output, as.vector))) %>% 
    rename(node_1 = V1, node_2 = V2)
  
  return(df_entity_text_output)
}