#' The following script documents the final function which executes all steps
#' of the package.


# Library ----------------------------------------------------------------------
library(dplyr)

#' IS NOT NULL
#'  Returns a boolean verifying is the provided input is not null

is.not.null <- function(x) !is.null(x)


#'  GENERATE OUTPUT TABLE
#'  
#'  INPUT
#'  - file_path: single or vector of paths to file name
#'  
#'  - folder_path : path to folder of pdfs
#'  
#'  only one input can be entered

gen_output_table <- function(file_path = NULL, folder_path = NULL) {
  
  # Generate File or List of Files
   
  pdf_path <- c()
  if (is.not.null(file_path)){
     
     pdf_paths <- file_path
     print(paste0("Input - File Method: ", pdf_paths))
       
   } else if (is.not.null(folder_path)) {
     
     pdf_paths <- list.files(recursive = FALSE, 
                             path = folder_path, 
                             pattern = ".pdf", 
                             full.names = TRUE)
     
     print(paste0("Input - Folder Method: ", pdf_paths))
   } else (
     print("File name(s) or folder path required.")
   )
  
  # Initialize 
  lst_output <- vector(mode = "list", length = length(pdf_paths))
  i = 1
  for (pdf in pdf_paths) {
    
    # Generate Final Table Components
    ## File Name
    file_name <- last(str_split(pdf, pattern = "/")[[1]])
    print(file_name)
    
    ## Text Pre-processing
    ### Wrap in Try to catch failed pdf to text conversions

    possible_error <- tryCatch({
      
      text_processed <- process_text(pdf)
      
    },
      
      error = function(e) { 
        e
        error_statement <- paste0("Error. File ", 
                                  file_name, 
                                  " could not be converted into text.")
        print(error_statement)
        
        }
      )
    ### Skip processing if error observed.
    if(inherits(possible_error, "error")) next    
    
    ## Hypothesis Classification
    hypothesis_df <- hypothesis_extraction(text_processed, fasttext_tag = FALSE)
    
    # Test if Empty
    hypothesis_empty_check <- hypothesis_df %>% pull(hypothesis)
    hypothesis_empty <- is_empty(hypothesis_empty_check)
    
    if (!(hypothesis_empty)) {
      
      ## Entity Extraction
      entities <- entity_extraction(hypothesis_df)
      
      # Test if Empty
      empty_entity_check <- entities %>% drop_na() %>% pull(cause)
      entity_empty <- is_empty(empty_entity_check)
      
      if (!(entity_empty)) {
        
        ## Causality Classification
        causality_class <- causality_classification(hypothesis_df)
        causality_class <- data.frame(causality_class)
        
        # Compile Table
        iter_df <- cbind(hypothesis_df, entities) %>% drop_na()
        iter_df <- cbind(iter_df, causality_class)
        iter_df$file_name <- file_name
        
        # Modify Headers and Format
        iter_df <- iter_df %>% 
          rename(
            hypothesis_num = h_id,
            causal_relationship = causality_pred
          ) %>% 
          select(
            file_name, hypothesis_num, hypothesis, cause, 
            effect, causal_relationship
          ) %>% 
          modify_if(is.factor, as.character)
        
        # Store in List
        lst_output[[i]] <- iter_df
        i <- i + 1
      }
    }
  }
  
  # Group Output Table for All Files into one table
  output_df <- bind_rows(lst_output)
  
  return(output_df)

}