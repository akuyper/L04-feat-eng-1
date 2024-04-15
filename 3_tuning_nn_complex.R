# L04 Feature Engineering I ----
# Single layer neural net tuning, complex imputation ----

# Load package(s) ----
library(tidyverse)
library(tidymodels)
library(tictoc)
library(here)

# Handle conflicts
tidymodels_prefer()

# parallel processing ----


# load resamples ----


# load preprocessing/recipe ----


# model specifications ----


# define workflow ----


# hyperparameter tuning values ----


# tune/fit workflow/model ----
tic.clearlog() # clear log
tic("USEFUL_NAME: MODEL_REC") # start clock

# tuning code in here

toc(log = TRUE)

# Extract runtime info
time_log <- tic.log(format = FALSE)

tictoc_mlp_complex <- tibble(
  model = time_log[[1]]$msg,
  start_time = time_log[[1]]$tic,
  end_time = time_log[[1]]$toc,
  runtime = end_time - start_time
)

# write out results (fitted/trained workflows & runtime info) ----

