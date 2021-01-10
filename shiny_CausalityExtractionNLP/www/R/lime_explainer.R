#' The following generates a LIME explainer for a Fasttext model.
#'

# Library ---------------------------------------------------------------------
library(lime)
library(fastTextR)
library(tokenizers)

# Inputs
path_ft_model = "./www/models/hypothesis_classification/fasttext_model.bin"
ft_model <- fastTextR::ft_load(path_ft_model)

path_training_data = "./www/data/fasttext/fasttext_dataset_training.txt"


# LIME FUNCTIONS -------------------------------------------------------
#' The following functions use Methods defined in the fastTextR library.
#

# LIME Predict Model - fastText
predict_model.fasttext <- function(x, newdata, type, ...) {
  res <- fastTextR::ft_predict(x, newdata = newdata, ...)
  switch(
    type,
    raw = fastTextR::ft_predict(x, newdata = newdata, rval = "dense") %>% 
      as.data.frame() %>% 
      mutate(
        Response = if_else(.[[1]] > .[[2]], '__label__1', '__label__0')
      ) %>% 
      select(Response),
    prob = fastTextR::ft_predict(x, newdata, x$nlabels, rval = "dense") %>% 
      as.data.frame() 
  )
} 

# Model Type - fastText
model_type.fasttext <- function(x, ...) 'classification'


# LIME Explainer --------------------------------------------------------------
import_lime_explainer <- function() {
  
  explainer <- lime::lime(
    path_training_data, 
    ft_model,
    bow = FALSE
  )
  return(explainer)
  
}