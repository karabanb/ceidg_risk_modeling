
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
  sample_n(40000) %>%
  mutate_at(c('MainAddressCounty',
              'MainAddressVoivodeship',
              'CorrespondenceAddressCounty',
              'CorrespondenceAddressVoivodeship'), toupper) %>%
  mutate_at(c('PKDMainSection',
              'PKDMainDivision',
              'PKDMainGroup',
              'PKDMainClass'
              ), replace_na, 'Empty field') %>%
  mutate_if(is.character, as.factor) %>%
  select(-index,
         -NIP,
         -RandomDate,
         -DateOfTerminationOrSuspension,
         -IsFax,
         -RandomDate,
         -MonthOfRandomDate,
         -QuarterOfRandomDate)
  

### MERGING RARE FACTOR LEVELS INTO 'OTHER' ############################################################################

