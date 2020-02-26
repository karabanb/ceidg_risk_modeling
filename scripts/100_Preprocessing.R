
### LIBRARIES ##########################################################################################################

library(feather)
library(tidyverse)
library(funModeling)
library(DataExplorer)

### LOADNING DATA ######################################################################################################

raw_data <- read_feather('data/000_ceidg_data.feather')


### CLEANING DATA ######################################################################################################

df_status(raw_data)


cleaned_data <- raw_data %>% 
  head(100) %>%
  mutate_at(c('MainAddressCounty',
              'MainAddressVoivodeship',
              'CorrespondenceAddressCounty',
              'CorrespondenceAddressVoivodeship'), toupper) %>%
  select(-index,
         -NIP,
         -RandomDate,
         -Citizenship,
         -DateOfTerminationOrSuspension) %>%
  mutate_if(is.character, as.factor) 


