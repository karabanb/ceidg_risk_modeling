
library(h2o)

# write.csv(cleaned_data, 'data/100_CleanedData.csv', row.names = FALSE)
# 
# h2o.init()
# 
# cleaned_data <- h2o.importFile('data/100_CleanedData.csv')
# 
# h2o.describe(cleaned_data)
# 
# to_factors <- c('PKDMainDivision', 'PKDMainGroup', 'PKDMainClass', 'QuarterOfStartingOfTheBusiness')
# cleaned_data[, to_factors] <- as.factor(cleaned_data[ , to_factors])
# 
# df_split <- h2o.splitFrame(cleaned_data, ratios = 0.8, seed = 42)
# 
# train <- df_split[[1]]
# test <- df_split[[2]]
# 
# target = 'Target'
# 
# aml <- h2o.automl(y = target,
#                   training_frame = train,
#                   validation_frame = test,
#                   max_runtime_secs = 60, 
#                   seed = 42)


vr1 = var_rank_info(cleaned_data, 'Target')

### Auto grouping ------------------------------------------------------------------------------------------------------

df_status(cleaned_data)


MainAddressVoivodeship = auto_grouping(cleaned_data,
                                       input = 'MainAddressVoivodeship',
                                       target = 'Target',
                                       n_groups = 14,
                                       model = 'hclust',
                                       seed = 42)

categ_analysis(cleaned_data, 'MainAddressVoivodeship', 'Target')
MainAddressVoivodeship$recateg_results

CorrespondenceAddressVoivodeship = auto_grouping(cleaned_data,
                                                 input = 'CorrespondenceAddressVoivodeship',
                                                 target = 'Target',
                                                 n_groups = 15,
                                                 model = 'hclust',
                                                 seed = 42)

categ_analysis(cleaned_data, 'CorrespondenceAddressVoivodeship', 'Target')
CorrespondenceAddressVoivodeship$recateg_results

PKDMainSection = auto_grouping(cleaned_data,
                               input = 'PKDMainSection',
                               target = 'Target',
                               n_groups = 12,
                               seed = 42)



PKDMainDivision = auto_grouping(cleaned_data,
                             input = 'PKDMainDivision',
                             target = 'Target',
                             model = 'hclust',
                             n_groups = 15,
                             seed = 42)

PKDMainGroup = auto_grouping(cleaned_data,
                            input = 'PKDMainGroup',
                            target = 'Target',
                            n_groups = 25,
                            model = 'hclust',
                            seed = 42)

PKDMainClass = auto_grouping(cleaned_data,
                             input = 'PKDMainClass',
                             target = 'Target',
                             model = 'hclust',
                             n_groups = 35,
                             seed = 42)

PKDMainClass$recateg_results
CorrespondenceAddressCounty = auto_grouping(cleaned_data,
                             input = 'CorrespondenceAddressCounty',
                             target = 'Target',
                             n_groups = 18,
                             seed = 42)

# categ_analysis(cleaned_data, 'CorrespondenceAddressCounty', 'Target')


cleaned_data_tmp <- cleaned_data %>%
  inner_join(MainAddressVoivodeship$df_equivalence) %>%
  inner_join(CorrespondenceAddressVoivodeship$df_equivalence) %>%
  inner_join(PKDMainSection$df_equivalence) %>%
  inner_join(PKDMainDivision$df_equivalence) %>%
  inner_join(PKDMainGroup$df_equivalence) %>%
  inner_join(CorrespondenceAddressCounty$df_equivalence) %>%
  inner_join(PKDMainClass$df_equivalence) %>%
  select(-MainAddressCounty,
         -MainAddressVoivodeship,
         -CorrespondenceAddressCounty,
         -CorrespondenceAddressVoivodeship,
         -PKDMainSection,
         -PKDMainClass,
         -PKDMainGroup,
         -PKDMainDivision,
         -NoOfUniquePKDClasses,
         -NoOfUniquePKDGroups
         # -NoOfUniquePKDDivisions
         ) %>% 
  mutate_if(is.character, as.factor) %>%
  mutate(QuarterOfStartingOfTheBusiness = as.factor(QuarterOfStartingOfTheBusiness))

save(cleaned_data_tmp, file = 'tmp/Sketchbook_cleaned.RData')

sampled_cleaned_df <- cleaned_data_tmp %>% 
  sample_n(1000000)
  

write.csv(sampled_cleaned_df, 'data/Sketchbook_CleanedData.csv', row.names = FALSE)

h2o.init()

cleaned_data <- h2o.importFile('data/Sketchbook_CleanedData.csv')

h2o.describe(cleaned_data)

to_factors <- 'QuarterOfStartingOfTheBusiness'
cleaned_data[, to_factors] <- as.factor(cleaned_data[ , to_factors])

df_split <- h2o.splitFrame(cleaned_data, ratios = 0.8, seed = 42)

train <- df_split[[1]]
test <- df_split[[2]]

target = 'Target'

aml <- h2o.automl(y = target,
                  training_frame = train,
                  leaderboard_frame = test,
                  balance_classes = TRUE,
                  include_algos = c('GBM', 'XGboost', 'GLM'),
                  keep_cross_validation_predictions = TRUE,
                  keep_cross_validation_models = TRUE,
                  max_runtime_secs = 600, 
                  stopping_metric = 'AUC',
                  seed = 42)


alb = aml@leaderboard

m = h2o.getModel(alb[1, 'model_id'])

h2o.varimp(m)
vr1 = cleaned_data_tmp  %>% var_rank_info('Target')

#### BASE GLM ##########################################################################################################



set.seed(242)

n <- nrow(sampled_cleaned_df)
ix_train <- sample(1: n, 0.7 * n)

sampled_cleaned_df$Target <- as.integer(sampled_cleaned_df$Target)

sampled_cleaned_df <- sampled_cleaned_df %>%
  select(-MonthOfStartingOfTheBusiness,
         -QuarterOfStartingOfTheBusiness,
         -PKDMainClass_rec)

?fct_drop()

m_glm <- glm(Target~ . -1,
             family = 'binomial', data = sampled_cleaned_df[ix_train, ])

m_glm <- step(m_glm, direction = 'backward')

vif(m_glm)

m1_pred_trn <- predict(m_glm, newdata = cleaned_data_tmp[ix_train,], type = 'response')
m1_pred_tst <- predict(m_glm, newdata = cleaned_data_tmp[-ix_train,], type = 'response')

MLmetrics::AUC(m1_pred_trn, cleaned_data_tmp[ix_train, 'Target'])
MLmetrics::AUC(m1_pred_tst, sampled_cleaned_df[-ix_train, 'Target'])

sampled_dummies <- dummify(data = sampled_cleaned_df, maxcat = 60)

sampled_dummies_model <- sampled_dummies %>% 
  select(MainAndCorrespondenceAreTheSame_FALSE,
        # MainAndCorrespondenceAreTheSame_TRUE,
         NoOfAdditionalPlaceOfTheBusiness,
         CommunityProperty_nie,
         HasLicences_TRUE,
         Sex_M,
         ShareholderInOtherCompanies_TRUE,
         NoOfUniquePKDSections,
         NoOfPastBusinesses,
         DurationOfExistenceInMonths,
         PKDMainGroup_rec_group_10,
         PKDMainGroup_rec_group_23,
         PKDMainGroup_rec_group_35,
         PKDMainGroup_rec_group_36,
         CorrespondenceAddressCounty_rec_group_19,
         CorrespondenceAddressCounty_rec_group_3,
         PKDMainClass_rec_group_30,
         PKDMainClass_rec_group_33,
         PKDMainClass_rec_group_43,
         PKDMainClass_rec_group_47,
         PKDMainClass_rec_group_48,
         PKDMainClass_rec_group_5,
         PKDMainClass_rec_group_52,
         PKDMainClass_rec_group_54,
         Target
         ) %>% as.data.frame()

m2_glm <- glm(Target~.-1, data = sampled_dummies_model, family = 'binomial')
m2_glm_step <- step(m2_glm,direction = 'backward')

vif(m2_glm_step)

m2_pred_trn <- predict(m2_glm, newdata = sampled_dummies_model[ix_train,], type = 'response')
m2_pred_tst <- predict(m2_glm, newdata = sampled_dummies_model[-ix_train,], type = 'response')

Metrics::auc(sampled_dummies_model[ix_train, 'Target'], m2_pred_trn)
Metrics::auc(sampled_dummies_model[-ix_train, 'Target'], m2_pred_tst)
