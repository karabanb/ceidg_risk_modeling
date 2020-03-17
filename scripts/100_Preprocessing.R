
### LIBRARIES ##########################################################################################################

library(feather)
library(tidyverse)
library(funModeling)

### LOADNING DATA ######################################################################################################

raw_data <- read_feather('data/000_ceidg_data.feather')


### CLEANING DATA ######################################################################################################

df_status(raw_data)

char_vars <- c('PKDMainSection', 'PKDMainDivision', 'PKDMainGroup', 'PKDMainClass', 'MainAddressCounty',
               'MainAddressVoivodeship', 'CorrespondenceAddressCounty', 'CorrespondenceAddressVoivodeship')

set.seed(42)
cleaned_data <- raw_data %>%
  # sample_n(1000) %>%
  filter(NIP == '1130106921') %>% 
  mutate(Target = as.integer(Target),
         QuarterOfStartingOfTheBusiness = paste0('Q', QuarterOfStartingOfTheBusiness)) %>% 
  mutate_at(char_vars, toupper) %>%
  mutate_at(char_vars, na_if, '') %>%
  mutate_at(char_vars, replace_na, 'EmptyField') %>%
  mutate_if(is.character, as.factor) %>%
  select(NIP,
         RandomDate,
         MonthOfStartingOfTheBusiness,
         QuarterOfStartingOfTheBusiness,
         MainAddressVoivodeship,
         MainAddressCounty,
         CorrespondenceAddressVoivodeship,
         CorrespondenceAddressCounty,
         MainAndCorrespondenceAreTheSame,
         DurationOfExistenceInMonths,
         NoOfAdditionalPlaceOfTheBusiness,
         IsPhoneNo,
         IsEmail,
         IsWWW,
         CommunityProperty,
         HasLicences,
         NoOfLicences,
         Sex,
         HasPolishCitizenship,
         ShareholderInOtherCompanies,
         PKDMainSection,
         PKDMainDivision,
         PKDMainGroup,
         PKDMainClass,
         NoOfUniquePKDSections,
         NoOfUniquePKDDivsions,
         NoOfUniquePKDGroups,
         NoOfUniquePKDClasses,
         Target
  )
  
rm(raw_data, char_vars)


### MERGING RARE FACTOR LEVELS INTO 'OTHER' ############################################################################

cleaned_data_lump <- cleaned_data %>%
  mutate('PKDMainSection' = fct_lump_prop(PKDMainSection, prop = 0.01),
         'PKDMainDivision' = fct_lump_prop(PKDMainDivision, prop = 0.01),
         'PKDMainGroup' = fct_lump_prop(PKDMainGroup, prop = 0.01),
         'PKDMainClass' = fct_lump_prop(PKDMainClass, prop = 0.01),
         'MainAddressVoivodeship' = fct_lump(MainAddressVoivodeship, n = 16),
         'CorrespondenceAddressVoivodeship' = fct_lump(CorrespondenceAddressVoivodeship, n = 16))

### MERGING FACTORS BY CLUSTERING ######################################################################################

cat_PKDMainSection <- categ_analysis(cleaned_data, 'PKDMainSection', 'Target')

grp_PKDMainSection <- auto_grouping(data = cleaned_data,
                                    input = 'PKDMainSection',
                                    target = 'Target',
                                    n_groups = 7,
                                    model = 'hclust')

cleaned_data_PKDSectionGrouped <- cleaned_data %>% 
  inner_join(grp_PKDMainSection$df_equivalence)


cat_PKDMainDivision <- categ_analysis(cleaned_data, 'PKDMainDivision', 'Target')

grp_PKDMainDivision <- auto_grouping(data = cleaned_data,
                                  input = 'PKDMainDivision',
                                  target = 'Target',
                                  n_groups = 14,
                                  model = 'hclust')

cleaned_data_PKDDivisionGrouped <- cleaned_data %>% 
  inner_join(grp_PKDMainDivision$df_equivalence)



cat_PKDMainGroup <- categ_analysis(cleaned_data, 'PKDMainGroup', 'Target')

grp_PKDMainGroup <- auto_grouping(data = cleaned_data,
                                  input = 'PKDMainGroup',
                                  target = 'Target',
                                  n_groups = 30,
                                  model = 'hclust')


cleaned_data_PKDGroupGrouped <- cleaned_data %>% 
  inner_join(grp_PKDMainGroup$df_equivalence)

cat_MainAddressCounty <- categ_analysis(cleaned_data, 'MainAddressCounty', 'Target')
grp_MainAddressCounty <- auto_grouping(data = cleaned_data,
                                       input = 'MainAddressCounty',
                                       target = 'Target',
                                       n_groups = 20,
                                       model = 'hclust')

cleaned_data_Grouped <- cleaned_data %>%
  inner_join(grp_PKDMainGroup$df_equivalence) %>%
  inner_join(grp_MainAddressCounty$df_equivalence)

