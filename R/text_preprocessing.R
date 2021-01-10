#' The following functions perform the initial text processing steps. This
#' processing is performed on all text inputs. Further, more specific
#' processing is applied for output component related to the underlying
#' academic paper.
#'

# Library ----------------------------------------------------------------------
library(dplyr)
library(stringr)
library(tokenizers)

# Data -------------------------------------------------------------------------
# Patterns File
patterns_col <- c("remove","comments")
patterns_raw <- read_excel(path = "./../data/patterns.xlsx", 
                           col_names = patterns_col)
removal_patterns <- patterns_raw %>% pull(remove)

# REGEX Strings ----------------------------------------------------------------
## Identify Letters
regex_letters <- '[a-zA-Z]'

## Identify IP Address
regex_ip <- "(?:[\\d]{1,3})\\.(?:[\\d]{1,3})\\.(?:[\\d]{1,3})\\.(?:[\\d]{1,3})"

## Identify Parenthesis
regex_parens <- "\\(([^()]+)\\)"

## Identify Hypothesis Formats
regex_hypo <- c('h[0-9]{1,3}[a-zA-Z]\\:',
                'H[0-9]{1,3}[a-zA-Z]\\:',
                'h[0-9]{1,3}[a-zA-Z]\\.',
                'H[0-9]{1,3}[a-zA-Z]\\.',
                'h[0-9]{1,3}[a-zA-Z]',
                'H[0-9]{1,3}[a-zA-Z]',
                'hypothesis [0-9]{1,3}[a-zA-Z]\\:',
                'Hypothesis [0-9]{1,3}[a-zA-Z]\\:',
                'hypothesis [0-9]{1,3}[a-zA-Z]\\.',
                'Hypothesis [0-9]{1,3}[a-zA-Z]\\.',
                'hypothesis [0-9]{1,3}[a-zA-Z]',
                'Hypothesis [0-9]{1,3}[a-zA-Z]',
                'h[0-9]{1,3}\\:',
                'H[0-9]{1,3}\\:',
                'h[0-9]{1,3}\\.',
                'H[0-9]{1,3}\\.',
                'h[0-9]{1,3}',
                'H[0-9]{1,3}',
                'hypothesis [0-9]{1,3}\\:',
                'Hypothesis [0-9]{1,3}\\:',
                'hypothesis [0-9]{1,3}\\.',
                'Hypothesis [0-9]{1,3}\\.',
                'hypothesis [0-9]{1,3}',
                'Hypothesis [0-9]{1,3}')

## Identify Numbers
regex_return_num <- "(\\d)+"



# Functions -------------------------------------------------------------------

# Generate Regex From Vector
# The following function takes a character vector and generates
# a regex string. The string with either identify and partial
# or exact match, based on input.


gen_regex <- function(input_vector, match){
  input_vector_exact <- c()
  if (match == "exact"){
    for (item in input_vector){
      item_exact <- paste0("^",item,"$")
      input_vector_exact <- append(input_vector_exact, item_exact)
    }
    # Reassign Input Variable
    input_vector <- input_vector_exact
  }
  regex_string <- paste0("\\b(", paste(input_vector, collapse="|"), ")\\b")

  return(regex_string)
}

#' Remove Line If String Detected
#' The following function removes a string if it matches to the
#' provided regex. If a regex string is not provided, a character
#' vector may be provided and converted into a regex string
#' which will identify any of the objects in the vector.
#'
#' Location input must be start, end, or any

remove_if_detect <- function(input_vector,
                             regex = NULL,
                             remove_vector = NULL,
                             location = "any",
                             match = "partial",
                             logical_method = "normal"){

  # Generate Regex Input Based on Vector If One Is Not Provided
  if (is.null(regex)){
    regex <- gen_regex(remove_vector, match)
  }

  logical_vector = c()
  if (location == "start"){
    # Check Start of String
    logical_vector <- str_starts(input_vector, regex)

  } else if ( location == "end") {
    # Check End of String
    logical_vector <- str_ends(input_vector, regex)

  } else {
    logical_vector <- str_detect(input_vector, regex)
  }


  # Drop Elements NOT Identified in Logical Vector
  # If Inverse Match is Selected, Elements that ARE Identified are dropped

  if (logical_method == "inverse") {
    output_vector <- input_vector[logical_vector]

  } else{
    output_vector <- input_vector[!logical_vector]
  }


  # Drop any NA
  output_vector <- output_vector[!is.na(output_vector)]

  return(output_vector)
}


#' Concatenate Two Strings, Drop Tail Hyphen of Initial String
concat_hyphen_string <- function(string_1, string_2){
  # Remove Last Hyphen
  string_1 <- str_sub(string_1, 1, nchar(string_1)-1)

  # Concatenate Strings
  output <- str_c(string_1, string_2)

  # Return Concatenated Strings
  return(output)
}

#' Execute **concat_hyphen_string** across Vector
concat_hypen_vector <- function(input){
  # Initialize
  i <- 1
  j <- 1
  output = c()

  while (i <= length(input)){
    item = input[i]
    # Test if Element Ends in Hyphen
    hyphen_test <- str_ends(item, "-")

    # Execute if Test = True
    while (hyphen_test){
      # Concatenate Element i with Element i+1
      item <- concat_hyphen_string(item, input[i+j])

      # Test if New Element Ends in Hyphen
      hyphen_test <- str_ends(item, "-")

      j = j + 1
    }
    output <- append(output, item)
    i = i + j
    j = 1
  }
  return(output)
}


#' Standardize Hypothesis
#' The following function searches for a hypotheseis in each vector element.
#' Hypotheses of different formats are identified based on a provided vector
#' of regex strings. All identified hypotheses are converted to a standard
#' format.

standardize_hypothesis <- function(input, regex_hypothesis_string){

  # Extract Identified Value
  extract_phrase <- str_extract(input, regex_hypothesis_string)

  # Check if Hypothesis Detected
  if (!is.na(extract_phrase)){

    # Extract Hypothesis Number
    extact_number <- str_extract(extract_phrase, regex_return_num)

    # Create New String
    replacement_string <- paste0("<split>Hypo ", extact_number, ": ")

    # Replace Hypothesis with New Value
    output_string <- str_replace(string = input,
                                 pattern = regex_hypothesis_string,
                                 replacement = replacement_string)

  } else{
    output_string <- input

  }

  return(output_string)

}

# Vectorize Function
standardize_hypothesis <- Vectorize(standardize_hypothesis)


#' Process Text
#' The following function executes all steps in the text
#' pre-processing steps.
#'

process_text <- function(input_text){

  # Vectorize -----------------------------------------------------------------
  ## Split Text into Character Vector
  processing_text <- input_text %>%
    str_split(pattern = "\r\n") %>% # Tabulizer
    str_split(pattern = "\n") %>% # Tika
    unlist()

  # References / Bibliography -------------------------------------------------
  ## Remove Anything From References / Bibliography to End
  ## Define Sections
  section_key <- c("References", "Bibliography",
                   "REFERENCES", "BIBIOGRAPHY")

  ## Convert to Regex String
  regex_section <- gen_regex(
    input_vector = section_key,
    match = "exact"
  )

  ## Return Logical Vector
  logical_section <- str_detect(processing_text, regex_section)

  ## Verify Any Elements Returned True
  ## Drop All Elements After First Instance if Any Element Returned True
  if (any(logical_section)){
    index <- min(which(logical_section == TRUE))
    processing_text <- processing_text[1:index-1]
  }


  # Removal Patterns ----------------------------------------------------------
  ## Remove Elements Which Match Removal Patterns
  processing_text <- processing_text[!processing_text %in% removal_patterns]

  # Numbers and Symbols -------------------------------------------------------
  ## Drop Lines With Only Numbers or Symbols
  processing_text <- remove_if_detect(
    input_vector = processing_text,
    regex_letters,
    logical_method = "inverse"
  )

  # n < 1 ---------------------------------------------------------------------
  ## Drop Elements with Length of 1
  ## Return Logical Vector
  logical_length <- nchar(processing_text) > 1

  ## Drop All Lines Length of 1 or Less
  processing_text <- processing_text[logical_length]

  ## Drop Any NA Elements
  processing_text <- processing_text[!is.na(processing_text)]

  # Months ---------------------------------------------------------------------
  ## Remove Elements which Start With Month
  processing_text <- remove_if_detect(
    input_vector = processing_text,
    remove_vector = toupper(month.name),
    location = "start"
  )

  ## Drop Any NA Elements
  processing_text <- processing_text[!is.na(processing_text)]

  # Hyphen Concatenation ------------------------------------------------------
  ## Concatenate Adjacent Elements If Initial Element Ends With Hyphen

  processing_text <- concat_hypen_vector(processing_text)

  # Downloading ---------------------------------------------------------------
  ## Remove Elements Which Contain Terms Related to Downloading

  download_vec <- c('This content downloaded','http','jsto','DOI','doi')

  processing_text <- remove_if_detect(
    input_vector = processing_text,
    remove_vector = download_vec,
    location = "any"
  )

  # IP Address ---------------------------------------------------------------
  ## Remove Elements Which Contain IP Addresses

  processing_text <- remove_if_detect(
    input_vector = processing_text,
    regex = regex_ip,
    location = "any"
  )

  # Parenthesis ---------------------------------------------------------------
  ## Remove Text Within Parenthesis
  ## Define Term to Identify Line Splits
  line_split_indicator <- " -LINESPLIT-"

  ## Concatenate All Vector Elements, Separated By Line Split Term
  processing_text <- str_c(processing_text,
                           collapse = line_split_indicator)

  # Remove Content Within Parenthesis
  processing_text <- str_remove_all(string = processing_text,
                                    pattern = regex_parens)

  # Split Single String Back into Character Vector
  processing_text <- str_split(string = processing_text,
                               pattern = line_split_indicator) %>%
    unlist()

  # Empty Vectors -------------------------------------------------------------
  ## Drop Empty Vectors
  processing_text <- processing_text[processing_text!=""]

  ## Drop NA Elements
  processing_text <- processing_text[!is.na(processing_text)]


  # Numbers and Symbols (Second Time) -----------------------------------------
  ## Drop Lines With Only Numbers or Symbols
  processing_text <- remove_if_detect(
    input_vector = processing_text,
    regex_letters,
    logical_method = "inverse"
  )

  # Tokenize Sentences --------------------------------------------------------
  ## Convert Vector Elements into Sentences
  processing_text <- str_c(processing_text, collapse = " ")
  processing_text <- tokenize_sentences(processing_text,
                                        strip_punct = FALSE) %>%
    unlist()

  ## Replace Double Spaces With Single
  processing_text <- str_replace_all(string = processing_text,
                                     pattern = "  ",
                                     replacement = " ")

  # Downloading (Second Time) -------------------------------------------------
  ## Remove Elements Which Contain Terms Related to Downloading

  processing_text <- remove_if_detect(
    input_vector = processing_text,
    remove_vector = download_vec,
    location = "any"
  )

  # Numbers and Symbols (Third Time) ------------------------------------------
  ## Drop Lines With Only Numbers or Symbols
  processing_text <- remove_if_detect(
    input_vector = processing_text,
    regex_letters,
    logical_method = "inverse"
  )

  # Standardize Hypothesis ----------------------------------------------------

  # Generate Regex String to ID Hypotheses
  regex_hypo_str <- gen_regex(regex_hypo, match = "partial")

  processing_text <- standardize_hypothesis(input = processing_text,
                                            regex_hypothesis_str = regex_hypo_str)
  ## Remove Names
  processing_text <- unname(processing_text)

  # # Misc Text Replacement -----------------------------------------------------
  ## Replace Double Colons
  processing_text <- str_replace_all(processing_text,
                                     pattern = ": :",
                                     replacement = ":")

  ## Remove Extra White Space
  processing_text <- str_squish(string = processing_text)

  ## Replace Colon Period Instances (: .)
  processing_text <- str_replace_all(processing_text,
                                     pattern = ": \\.",
                                     replacement = ":")

  return(processing_text)
}
