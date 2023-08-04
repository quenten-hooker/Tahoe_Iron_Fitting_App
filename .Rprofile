# This file configures the virtualenv and Python paths differently depending on
# the environment the app is running in (local vs remote server).

# Edit this name if desired when starting a new app
VIRTUALENV_NAME = 'example_env_name'

# ------------------------- Settings (Edit local settings to match your system) -------------------------- #

if (Sys.info()[['user']] == 'shiny'){

  # Running on shinyapps.io
  Sys.setenv(PYTHON_PATH = 'python3')
  Sys.setenv(VIRTUALENV_NAME = VIRTUALENV_NAME) # Installs into default shiny virtualenvs dir
  Sys.setenv(RETICULATE_PYTHON = paste0('/home/shiny/.virtualenvs/', VIRTUALENV_NAME, '/bin/python'))
  reticulate::use_virtualenv("/cloud/project/python3_env/", required = TRUE)
  
  #Sys.setenv(RETICULATE_PYTHON = "~/.virtualenvs/python35_env/bin/python")
  
  
  
} else if (Sys.info()[['user']] == 'rstudio-connect'){

  # Running on remote server
  Sys.setenv(PYTHON_PATH = '/opt/python/3.7.7/bin/python3')
  Sys.setenv(VIRTUALENV_NAME = paste0(VIRTUALENV_NAME, '/')) # include '/' => installs into rstudio-connect/apps/
  Sys.setenv(RETICULATE_PYTHON = paste0(VIRTUALENV_NAME, '/bin/python'))

} else {

  # Running locally
  options(shiny.port = 7450)
  Sys.setenv(PYTHON_PATH = 'C:/Users/Quenten.hooker/Documents_unlink/.virtualenvs/example_env_name/Scripts/')
  Sys.setenv(VIRTUALENV_NAME = VIRTUALENV_NAME)
  #Sys.setenv(RETICULATE_PYTHON = paste0('/home/shiny/.virtualenvs/', VIRTUALENV_NAME, '/bin/python'))# exclude '/' => installs into ~/.virtualenvs/
  # RETICULATE_PYTHON is not required locally, RStudio infers it based on the ~/.virtualenvs path
}


