# This file configures the virtualenv and Python paths differently depending on
# the environment the app is running in (local vs remote server).

# Edit this name if desired when starting a new app
VIRTUALENV_NAME = 'CausalityExtractionNLP_app'


# ------------------------- Settings (Do not edit) -------------------------- #

if (Sys.info()[['user']] == 'shiny'){
  
  # Running on shinyapps.io
  Sys.setenv(PYTHON_PATH = 'python3')
  Sys.setenv(VIRTUALENV_NAME = VIRTUALENV_NAME) # Installs into default dir
  Sys.setenv(RETICULATE_PYTHON = paste0('/home/shiny/.virtualenvs/', 
                                        VIRTUALENV_NAME, '/bin/python'))
  
} else if (Sys.info()[['user']] == 'rstudio-connect'){
  
  # Running on remote server
  Sys.setenv(PYTHON_PATH = '/opt/python/3.7.6/bin/python')
  Sys.setenv(VIRTUALENV_NAME = paste0(VIRTUALENV_NAME, '/')) 
  Sys.setenv(RETICULATE_PYTHON = paste0(VIRTUALENV_NAME, '/bin/python'))
  
} else {
  
  # Running locally
  options(shiny.port = 7450)
  Sys.setenv(PYTHON_PATH = 'python')
  Sys.setenv(VIRTUALENV_NAME = VIRTUALENV_NAME) 
  # RETICULATE_PYTHON is not required locally, 
  # RStudio infers it based on the ~/.virtualenvs path
}
