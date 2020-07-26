library(tidymodels)
library(tidyverse)
library(here)

# Data Pull
churn_dataset <- read.csv(here::here('Data/ge_cell_data.csv'))

# Calibration and Validation Table Set Up

churn_calibration <- churn_dataset %>% filter(CALIBRAT == 1)
churn_validation <- churn_dataset %>% filter(CALIBRAT == 0)

