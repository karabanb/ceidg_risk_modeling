
library(DataExplorer)
library(car)
library(dummies)
library(scorecard)

cleaned_data_lump <- cleaned_data %>%
  mutate('PKDMainSection' = fct_lump_prop(PKDMainSection, prop = 0.01),
         'PKDMainDivision' = fct_lump_prop(PKDMainDivision, prop = 0.01),
         'PKDMainGroup' = fct_lump_prop(PKDMainGroup, prop = 0.01),
         'PKDMainClass' = fct_lump_prop(PKDMainClass, prop = 0.01),
         'MainAddressVoivodeship' = fct_lump(MainAddressVoivodeship, n = 16),
         'CorrespondenceAddressVoivodeship' = fct_lump(CorrespondenceAddressVoivodeship, n = 16))

### Different levels of modularity of the data -------------------------------------------------------------------------

sample_size = 100000

set.seed(42)
dummy_PKDSection <- cleaned_data_lump %>%
  sample_n(sample_size) %>% 
  select(-PKDMainDivision,
         -PKDMainGroup,
         -PKDMainClass,
         -CorrespondenceAddressVoivodeship,
         -MainAddressVoivodeship,
         -MonthOfStartingOfTheBusiness,
         -QuarterOfStartingOfTheBusiness,
         -NoOfLicences,
         -NoOfUniquePKDGroups,
         -NoOfUniquePKDDivsions,
         -NoOfUniquePKDClasses,
         -NoOfCitizenships) %>%
  dummify(maxcat = 100, select = setdiff(colnames(.), 'Target')) %>% 
  select(-ends_with('FALSE'))


set.seed(42)
dummy_PKDDivision <- cleaned_data_lump %>%
  sample_n(sample_size) %>% 
  select(-PKDMainSection,
         -PKDMainGroup,
         -PKDMainClass,
         -CorrespondenceAddressVoivodeship,
         -MainAddressVoivodeship,
         -MonthOfStartingOfTheBusiness,
         -QuarterOfStartingOfTheBusiness,
         -NoOfLicences,
         -NoOfUniquePKDGroups,
         -NoOfUniquePKDDivsions,
         -NoOfUniquePKDClasses,
         -NoOfCitizenships) %>%
  dummify(maxcat = 100, select = setdiff(colnames(.), 'Target')) %>% 
  select(-ends_with('FALSE'))


set.seed(42)
dummy_PKDGroup <- cleaned_data_lump %>%
  sample_n(sample_size) %>% 
  select(
         -PKDMainSection,
         -PKDMainDivision,
         -PKDMainClass,
         -CorrespondenceAddressVoivodeship,
         -MainAddressVoivodeship,
         -MonthOfStartingOfTheBusiness,
         -QuarterOfStartingOfTheBusiness,
         -NoOfLicences,
         -NoOfUniquePKDGroups,
         -NoOfUniquePKDDivsions,
         -NoOfUniquePKDClasses,
         -NoOfCitizenships) %>%
  mutate(NoOfAdditionalPlaceOfTheBusiness = sqrt(NoOfAdditionalPlaceOfTheBusiness)) %>% 
  dummify(maxcat = 100, select = setdiff(colnames(.), 'Target')) %>% 
  select(-ends_with('FALSE'))


set.seed(42)
dummy_PKDClass <- cleaned_data_lump %>%
  sample_n(sample_size) %>% 
  select(
    -PKDMainSection,
    -PKDMainDivision,
    -PKDMainGroup,
    -CorrespondenceAddressVoivodeship,
    -MainAddressVoivodeship,
    -MonthOfStartingOfTheBusiness,
    -QuarterOfStartingOfTheBusiness,
    -NoOfLicences,
    -NoOfUniquePKDGroups,
    -NoOfUniquePKDDivsions,
    -NoOfUniquePKDClasses,
    -NoOfCitizenships) %>%
  dummify(maxcat = 100, select = setdiff(colnames(.), 'Target')) %>% 
  select(-ends_with('FALSE'))

rm(cleaned_data_lump, cleaned_data)

set.seed(42)
dummy_PKDSectionGrouped <- cleaned_data_PKDSectionGrouped %>%
  sample_n(sample_size) %>% 
  select(
    -PKDMainSection,
    -PKDMainDivision,
    -PKDMainGroup,
    -PKDMainClass,
    -CorrespondenceAddressVoivodeship,
    -MainAddressVoivodeship,
    -MonthOfStartingOfTheBusiness,
    -QuarterOfStartingOfTheBusiness,
    -NoOfLicences,
    -NoOfUniquePKDGroups,
    -NoOfUniquePKDDivsions,
    -NoOfUniquePKDClasses,
    -NoOfCitizenships) %>%
  dummify(maxcat = 100, select = setdiff(colnames(.), 'Target')) %>% 
  select(-ends_with('FALSE'),
         -PKDMainSection_rec_group_5
         )

rm(cleaned_data_PKDSectionGrouped)


set.seed(42)
dummy_PKDDivisionGrouped <- cleaned_data_PKDDivisionGrouped %>%
  sample_n(sample_size) %>% 
  select(
    -PKDMainSection,
    -PKDMainDivision,
    -PKDMainGroup,
    -PKDMainClass,
    -CorrespondenceAddressVoivodeship,
    -MainAddressVoivodeship,
    -MonthOfStartingOfTheBusiness,
    -QuarterOfStartingOfTheBusiness,
    -NoOfLicences,
    -NoOfUniquePKDGroups,
    -NoOfUniquePKDDivsions,
    -NoOfUniquePKDClasses,
    -NoOfCitizenships) %>%
  dummify(maxcat = 100, select = setdiff(colnames(.), 'Target')) %>% 
  select(-ends_with('FALSE'),
         -PKDMainDivision_rec_group_8
  )

rm(cleaned_data_PKDDivisionGrouped)

set.seed(42)
dummy_PKDGroupGrouped <- cleaned_data_PKDGroupGrouped %>%
  sample_n(sample_size) %>% 
  select(
    -PKDMainSection,
    -PKDMainDivision,
    -PKDMainGroup,
    -PKDMainClass,
    -CorrespondenceAddressVoivodeship,
    -MainAddressVoivodeship,
    -MonthOfStartingOfTheBusiness,
    -QuarterOfStartingOfTheBusiness,
    -NoOfLicences,
    -NoOfUniquePKDGroups,
    -NoOfUniquePKDDivsions,
    -NoOfUniquePKDClasses,
    -NoOfCitizenships) %>%
  dummify(maxcat = 100, select = setdiff(colnames(.), 'Target')) %>% 
  select(-ends_with('FALSE')) 
  

rm(cleaned_data_PKDGroupGrouped)

set.seed(42)
dummy_Grouped <- cleaned_data_Grouped %>%
  sample_n(sample_size) %>% 
  select(
    -PKDMainSection,
    -PKDMainDivision,
    -PKDMainGroup,
    -PKDMainClass,
    -CorrespondenceAddressVoivodeship,
    -MainAddressVoivodeship,
    -MainAddressCounty,
    -MonthOfStartingOfTheBusiness,
    -QuarterOfStartingOfTheBusiness,
    -NoOfLicences,
    -NoOfUniquePKDGroups,
    -NoOfUniquePKDDivsions,
    -NoOfUniquePKDClasses,
    -NoOfCitizenships) %>%
  dummify(maxcat = 100, select = setdiff(colnames(.), 'Target')) %>% 
  select(-ends_with('FALSE')
  )

rm(cleaned_data_Grouped)

set.seed(42)
ix_train <- sample(1:sample_size, 0.7 * sample_size)
weights <- if_else(dummy_PKDSection$Target == 1, 4, 1) 

### GLM witg different datasets ########################################################################################

### GLM PKDMainSection -------------------------------------------------------------------------------------------------

m1_glm <- glm(Target~., 
              data = dummy_PKDSection[ix_train,],
              family = 'binomial',
              weights = weights[ix_train])


m1_glm_bic <- step(m1_glm, direction = 'both', k = log(sample_size * 0.7))
vif(m1_glm_bic)

pred_m1_bic_trn <- predict(m1_glm_bic, newdata = dummy_PKDSection[ix_train,], type='response')
pred_m1_bic_tst <- predict(m1_glm_bic, newdata = dummy_PKDSection[-ix_train,], type='response')

MLmetrics::AUC(pred_m1_bic_trn, dummy_PKDSection$Target[ix_train])
MLmetrics::AUC(pred_m1_bic_tst, dummy_PKDSection$Target[-ix_train])

m1_glm_r <- glm(Target ~ NoOfAdditionalPlaceOfTheBusiness + NoOfUniquePKDSections + 
                  NoOfPastBusinesses + DurationOfExistenceInMonths + IsWWW_TRUE + 
                  CommunityProperty_nie + HasLicences_TRUE + 
                  Sex_F + HasPolishCitizenship_TRUE + ShareholderInOtherCompanies_TRUE + 
                  PKDMainSection_A + PKDMainSection_C + PKDMainSection_R +
                  PKDMainSection_G + PKDMainSection_H + PKDMainSection_I + 
                  PKDMainSection_J + PKDMainSection_N + PKDMainSection_Q,
                data = dummy_PKDSection[ix_train,],
                family = 'binomial',
                weights = weights[ix_train])


pred_m1_r_trn <- predict(m1_glm_r, newdata = dummy_PKDSection[ix_train,], type='response')
pred_m1_r_tst <- predict(m1_glm_r, newdata = dummy_PKDSection[-ix_train,], type='response')

MLmetrics::AUC(pred_m1_r_trn, dummy_PKDSection$Target[ix_train]) #AUC 67.3 [100k]
MLmetrics::AUC(pred_m1_r_tst, dummy_PKDSection$Target[-ix_train]) #AUC 67.5 [100k]

### GLM PKDMainDivision ------------------------------------------------------------------------------------------------

m2_glm <- glm(Target~., 
              data = dummy_PKDDivision[ix_train,],
              family = 'binomial',
              weights = weights[ix_train])


m2_glm_bic <- step(m2_glm, direction = 'both', k = log(sample_size * 0.7))
vif(m2_glm_bic)

pred_m2_bic_trn <- predict(m2_glm_bic, newdata = dummy_PKDDivision[ix_train,], type='response')
pred_m2_bic_tst <- predict(m2_glm_bic, newdata = dummy_PKDDivision[-ix_train,], type='response')

MLmetrics::AUC(pred_m2_bic_trn, dummy_PKDDivision$Target[ix_train]) # AUC 68.4 [100k]
MLmetrics::AUC(pred_m2_bic_tst, dummy_PKDDivision$Target[-ix_train]) #AUC 68.3 [100k]

m2_glm_r <- glm(Target ~ NoOfAdditionalPlaceOfTheBusiness + NoOfUniquePKDSections + 
                  NoOfPastBusinesses + DurationOfExistenceInMonths + IsWWW_TRUE + 
                  CommunityProperty_nie + HasLicences_TRUE + 
                  Sex_F + HasPolishCitizenship_TRUE + ShareholderInOtherCompanies_TRUE + 
                  PKDMainDivision_33 + PKDMainDivision_45 + PKDMainDivision_47 + 
                  PKDMainDivision_49 + PKDMainDivision_56 + PKDMainDivision_62 + 
                  PKDMainDivision_66 + PKDMainDivision_68 + PKDMainDivision_69 + 
                  PKDMainDivision_70 + PKDMainDivision_71 + PKDMainDivision_73 + 
                  PKDMainDivision_74 + PKDMainDivision_85 + PKDMainDivision_86 + 
                  PKDMainDivision_96,
                data = dummy_PKDDivision[ix_train,],
                weights = weights[ix_train])

pred_m2_r_trn <- predict(m2_glm_r, newdata = dummy_PKDDivision[ix_train,], type='response')
pred_m2_r_tst <- predict(m2_glm_r, newdata = dummy_PKDDivision[-ix_train,], type='response')

MLmetrics::AUC(pred_m2_r_trn, dummy_PKDDivision$Target[ix_train]) # AUC 68.3 [100k]
MLmetrics::AUC(pred_m2_r_tst, dummy_PKDDivision$Target[-ix_train]) #AUC 68.2 [100k]

### GLM PKDMainGroup ---------------------------------------------------------------------------------------------------

m3_glm <- glm(Target~., 
             data = dummy_PKDGroup[ix_train,],
             family = 'binomial',
             weights = weights[ix_train])

m3_glm_bic <- step(m3_glm, direction = 'both', k = log(sample_size * 0.7))
vif(m3_glm_bic)

summary(m3_glm_bic)

pred_m3_bic_trn <- predict(m3_glm_bic, newdata = dummy_PKDGroup[ix_train,], type='response')
pred_m3_bic_tst <- predict(m3_glm_bic, newdata = dummy_PKDGroup[-ix_train,], type='response')

MLmetrics::AUC(pred_m3_bic_trn, dummy_PKDGroup$Target[ix_train]) # AUC 68.6 [100k]
MLmetrics::AUC(pred_m3_bic_tst, dummy_PKDGroup$Target[-ix_train]) # AUC 68.6 [100k]

m3_glm_r <- glm(Target ~ NoOfAdditionalPlaceOfTheBusiness + NoOfUniquePKDSections + 
                  NoOfPastBusinesses + DurationOfExistenceInMonths + IsWWW_TRUE + 
                  CommunityProperty_nie + HasLicences_TRUE + 
                  Sex_F + HasPolishCitizenship_TRUE + ShareholderInOtherCompanies_TRUE + 
                  PKDMainGroup_331 + PKDMainGroup_432 + PKDMainGroup_452 + 
                  PKDMainGroup_471 + PKDMainGroup_477 + PKDMainGroup_478 + 
                  PKDMainGroup_479 + PKDMainGroup_493 + PKDMainGroup_494 + 
                  PKDMainGroup_561 + PKDMainGroup_620 + PKDMainGroup_662 + 
                  PKDMainGroup_691 + PKDMainGroup_692 + 
                  PKDMainGroup_711 + PKDMainGroup_731 + 
                  PKDMainGroup_862 + PKDMainGroup_869 + PKDMainGroup_960,
                data = dummy_PKDGroup[ix_train,],
                family = 'binomial',
                weights = weights[ix_train])

summary(m3_glm_r)

pred_m3_r_trn <- predict(m3_glm_r, newdata = dummy_PKDGroup[ix_train,], type='response')
pred_m3_r_tst <- predict(m3_glm_r, newdata = dummy_PKDGroup[-ix_train,], type='response')

MLmetrics::AUC(pred_m3_r_trn, dummy_PKDGroup$Target[ix_train]) # AUC 68.5 [100k]
MLmetrics::AUC(pred_m3_r_tst, dummy_PKDGroup$Target[-ix_train]) # AUC 68.6 [100k]


# --  5 fold CV ----

cv_m3_r <- perf_cv(dummy_PKDGroup,
                   x = setdiff(names(m3_glm_r$coefficients), c('Target', '(Intercept)')),
                   y = 'Target',
                   no_folds = 10,
                   binomial_metric = 'auc')

### GLM PKDMainClass ---------------------------------------------------------------------------------------------------

m4_glm <- glm(Target~., 
              data = dummy_PKDClass[ix_train,],
              family = 'binomial',
              weights = weights[ix_train])

m4_glm_bic <- step(m4_glm, direction = 'both', k = log(sample_size * 0.7))
vif(m4_glm_bic)

summary(m4_glm_bic)

pred_m4_bic_trn <- predict(m4_glm_bic, newdata = dummy_PKDClass[ix_train,], type='response')
pred_m4_bic_tst <- predict(m4_glm_bic, newdata = dummy_PKDClass[-ix_train,], type='response')

MLmetrics::AUC(pred_m4_bic_trn, dummy_PKDClass$Target[ix_train]) # AUC 68.2 [100k]
MLmetrics::AUC(pred_m4_bic_tst, dummy_PKDClass$Target[-ix_train]) # AUC 68.3 [100k]


m4_glm_r <- glm(Target ~ NoOfAdditionalPlaceOfTheBusiness + NoOfUniquePKDSections + 
                  NoOfPastBusinesses + DurationOfExistenceInMonths + IsWWW_TRUE + 
                  CommunityProperty_nie + HasLicences_TRUE + 
                  Sex_F + HasPolishCitizenship_TRUE + ShareholderInOtherCompanies_TRUE + 
                  PKDMainClass_4322 + PKDMainClass_4339 + 
                  PKDMainClass_4520 + PKDMainClass_4711 + PKDMainClass_4771 + 
                  PKDMainClass_4782 + PKDMainClass_4791 + PKDMainClass_4932 + 
                  PKDMainClass_4941 + PKDMainClass_5610 + PKDMainClass_6201 + 
                  PKDMainClass_6622 + PKDMainClass_6910 + PKDMainClass_6920 + 
                  PKDMainClass_7112 + PKDMainClass_7311 + 
                  PKDMainClass_8621 + PKDMainClass_8622 + PKDMainClass_8690 + 
                  PKDMainClass_9602,
                data = dummy_PKDClass[ix_train,],
                weights = weights[ix_train])
vif(m4_glm_r)

pred_m4_r_trn <- predict(m4_glm_r, newdata = dummy_PKDClass[ix_train,], type='response')
pred_m4_r_tst <- predict(m4_glm_r, newdata = dummy_PKDClass[-ix_train,], type='response')

MLmetrics::AUC(pred_m4_r_trn, dummy_PKDClass$Target[ix_train]) # AUC 68 [100k]
MLmetrics::AUC(pred_m4_r_tst, dummy_PKDClass$Target[-ix_train]) # AUC 68.2 [100k]


### GLM PKDMainSectionGrouped ------------------------------------------------------------------------------------------

m5_glm <- glm(Target~., 
              data = dummy_PKDSectionGrouped[ix_train,],
              family = 'binomial',
              weights = weights[ix_train],)

m5_glm_bic <- step(m5_glm, direction = 'both',
                   k = log(sample_size * 0.7)
                   )
vif(m5_glm_bic)

summary(m5_glm_bic)

pred_m5_bic_trn <- predict(m5_glm_bic, newdata = dummy_PKDSectionGrouped[ix_train,], type='response')
pred_m5_bic_tst <- predict(m5_glm_bic, newdata = dummy_PKDSectionGrouped[-ix_train,], type='response')

MLmetrics::AUC(pred_m5_bic_trn, dummy_PKDSectionGrouped$Target[ix_train])  # AUC 67 [100k]
MLmetrics::AUC(pred_m5_bic_tst, dummy_PKDSectionGrouped$Target[-ix_train]) # AUC 66.7 [100k]

m5_glm_r <- glm(Target ~ NoOfAdditionalPlaceOfTheBusiness + NoOfUniquePKDSections + 
                  NoOfPastBusinesses + DurationOfExistenceInMonths + IsWWW_TRUE + 
                  CommunityProperty_nie + HasLicences_TRUE + 
                  Sex_F + HasPolishCitizenship_TRUE + ShareholderInOtherCompanies_TRUE + 
                  PKDMainSection_rec_group_1 + PKDMainSection_rec_group_2 + 
                  PKDMainSection_rec_group_3 + PKDMainSection_rec_group_6 + 
                  PKDMainSection_rec_group_7,
                data = dummy_PKDSectionGrouped[ix_train,],
                weights = weights[ix_train])

vif(m5_glm_r)
summary(m5_glm_r)

pred_m5_r_trn <- predict(m5_glm_r, newdata = dummy_PKDSectionGrouped[ix_train,], type='response')
pred_m5_r_tst <- predict(m5_glm_r, newdata = dummy_PKDSectionGrouped[-ix_train,], type='response')

MLmetrics::AUC(pred_m5_r_trn, dummy_PKDClass$Target[ix_train])  # AUC 66.8 [100k]
MLmetrics::AUC(pred_m5_r_tst, dummy_PKDClass$Target[-ix_train]) # AUC 66.7 [100k]


### GLM PKDMainDivisionGrouped -----------------------------------------------------------------------------------------

m6_glm <- glm(Target~., 
              data = dummy_PKDDivisionGrouped[ix_train,],
              family = 'binomial',
              weights = weights[ix_train],)

m6_glm_bic <- step(m6_glm, direction = 'both',
                   k = log(sample_size * 0.7)
)

summary(m6_glm_bic)
vif(m6_glm_bic)

pred_m6_bic_trn <- predict(m6_glm_bic, newdata = dummy_PKDDivisionGrouped[ix_train,], type='response')
pred_m6_bic_tst <- predict(m6_glm_bic, newdata = dummy_PKDDivisionGrouped[-ix_train,], type='response')

MLmetrics::AUC(pred_m6_bic_trn, dummy_PKDDivisionGrouped$Target[ix_train]) # AUC 68.1 [100k]
MLmetrics::AUC(pred_m6_bic_tst, dummy_PKDDivisionGrouped$Target[-ix_train]) # AUC 68.2 [100k]

m6_glm_r <- glm(Target ~ NoOfAdditionalPlaceOfTheBusiness + NoOfUniquePKDSections + 
                NoOfPastBusinesses + DurationOfExistenceInMonths + IsWWW_TRUE + 
                CommunityProperty_nie + HasLicences_TRUE + 
                Sex_F + HasPolishCitizenship_TRUE + ShareholderInOtherCompanies_TRUE + 
                PKDMainDivision_rec_group_1 + PKDMainDivision_rec_group_11 + 
                PKDMainDivision_rec_group_12 + PKDMainDivision_rec_group_13 + 
                PKDMainDivision_rec_group_14 + PKDMainDivision_rec_group_4 + 
                PKDMainDivision_rec_group_6 + PKDMainDivision_rec_group_9, 
              data = dummy_PKDDivisionGrouped[ix_train,],
              family = 'binomial',
              weights = weights[ix_train],)

vif(m6_glm_r)
summary(m6_glm_r)

pred_m6_r_trn <- predict(m6_glm_r, newdata = dummy_PKDDivisionGrouped[ix_train,], type='response')
pred_m6_r_tst <- predict(m6_glm_r, newdata = dummy_PKDDivisionGrouped[-ix_train,], type='response')

MLmetrics::AUC(pred_m6_r_trn, dummy_PKDDivisionGrouped$Target[ix_train]) # AUC 68.1 [100k]
MLmetrics::AUC(pred_m6_r_tst, dummy_PKDDivisionGrouped$Target[-ix_train]) # AUC 68.2 [100k]

### GLM PKDMainGrpupGrouped -----------------------------------------------------------------------------------------

m7_glm <- glm(Target~., 
              data = dummy_PKDGroupGrouped[ix_train,],
              family = 'binomial',
              weights = weights[ix_train],)

m7_glm_bic <- step(m7_glm, direction = 'both',
                   k = log(sample_size * 0.7)
)

summary(m7_glm_bic)
vif(m7_glm_bic)

pred_m7_bic_trn <- predict(m7_glm_bic, newdata = dummy_PKDGroupGrouped[ix_train,], type='response')
pred_m7_bic_tst <- predict(m7_glm_bic, newdata = dummy_PKDGroupGrouped[-ix_train,], type='response')

MLmetrics::AUC(pred_m7_bic_trn, dummy_PKDGroup$Target[ix_train]) # AUC 68.7 [100k]
MLmetrics::AUC(pred_m7_bic_tst, dummy_PKDGroupGrouped$Target[-ix_train]) # AUC 68.7 [100k]

m7_glm_r <- glm(Target ~ NoOfAdditionalPlaceOfTheBusiness + NoOfUniquePKDSections + 
                  NoOfPastBusinesses + DurationOfExistenceInMonths + IsWWW_TRUE + 
                  CommunityProperty_nie + HasLicences_TRUE + 
                  Sex_F + HasPolishCitizenship_TRUE + ShareholderInOtherCompanies_TRUE + 
                  PKDMainGroup_rec_group_13 + PKDMainGroup_rec_group_14 + PKDMainGroup_rec_group_15 + 
                  PKDMainGroup_rec_group_16 + PKDMainGroup_rec_group_18 + PKDMainGroup_rec_group_19 + 
                  PKDMainGroup_rec_group_20 + PKDMainGroup_rec_group_21 + PKDMainGroup_rec_group_22 + 
                  PKDMainGroup_rec_group_23 + PKDMainGroup_rec_group_24 + PKDMainGroup_rec_group_25 + 
                  PKDMainGroup_rec_group_26 + PKDMainGroup_rec_group_27 + PKDMainGroup_rec_group_28 + 
                  PKDMainGroup_rec_group_29 + PKDMainGroup_rec_group_3 + PKDMainGroup_rec_group_30,
                data = dummy_PKDGroupGrouped[ix_train,],
                family = 'binomial',
                weights = weights[ix_train]
)

vif(m7_glm_r)
summary(m7_glm_r)
length(m7_glm_r$coefficients)

pred_m7_r_trn <- predict(m7_glm_r, newdata = dummy_PKDGroupGrouped[ix_train,], type='response')
pred_m7_r_tst <- predict(m7_glm_r, newdata = dummy_PKDGroupGrouped[-ix_train,], type='response')

MLmetrics::AUC(pred_m7_r_trn, dummy_PKDGroup$Target[ix_train]) # AUC 68.6 [100k]
MLmetrics::AUC(pred_m7_r_tst, dummy_PKDGroupGrouped$Target[-ix_train]) # AUC 68.5 [100k]


### GLM Grouped PKD Groups And Counties---------------------------------------------------------------------------------

m8_glm <- glm(Target~.,
              data = dummy_Grouped[ix_train,],
              family = 'binomial',
              weights = weights[ix_train])

m8_glm_bic <- step(m8_glm, direction = 'both',
                   k = log(sample_size * 0.7)
)

summary(m8_glm_bic)
vif(m8_glm_bic)

pred_m8_bic_trn <- predict(m8_glm_bic, newdata = dummy_Grouped[ix_train,], type='response')
pred_m8_bic_tst <- predict(m8_glm_bic, newdata = dummy_Grouped[-ix_train,], type='response')

MLmetrics::AUC(pred_m8_bic_trn, dummy_PKDGroup$Target[ix_train]) # AUC 68.7 [100k]
MLmetrics::AUC(pred_m8_bic_tst, dummy_PKDGroupGrouped$Target[-ix_train]) # AUC 68.7 [100k]


m8_glm_r <- glm(Target ~ NoOfAdditionalPlaceOfTheBusiness + NoOfUniquePKDSections + 
                  NoOfPastBusinesses + DurationOfExistenceInMonths + IsWWW_TRUE + 
                  CommunityProperty_nie + HasLicences_TRUE + 
                  Sex_F + HasPolishCitizenship_TRUE + ShareholderInOtherCompanies_TRUE + 
                  # PKDMainGroup_rec_group_11 + PKDMainGroup_rec_group_12 + 
                  PKDMainGroup_rec_group_13 + PKDMainGroup_rec_group_14 + PKDMainGroup_rec_group_15 + 
                  PKDMainGroup_rec_group_16 + PKDMainGroup_rec_group_18 + PKDMainGroup_rec_group_19 + 
                  PKDMainGroup_rec_group_20 + PKDMainGroup_rec_group_21 + PKDMainGroup_rec_group_22 + 
                  PKDMainGroup_rec_group_23 + PKDMainGroup_rec_group_24 + PKDMainGroup_rec_group_25 + 
                  PKDMainGroup_rec_group_26 + PKDMainGroup_rec_group_27 + PKDMainGroup_rec_group_28 + 
                  PKDMainGroup_rec_group_29 + PKDMainGroup_rec_group_3 + PKDMainGroup_rec_group_30 + 
                  # PKDMainGroup_rec_group_8 + 
                  MainAddressCounty_rec_group_12,
                data = dummy_Grouped[ix_train,],
                family = 'binomial',
                weights = weights[ix_train]
  
)

summary(m8_glm_r)
length(m8_glm_r$coefficients)

pred_m8_r_trn <- predict(m8_glm_r, newdata = dummy_Grouped[ix_train,], type='response')
pred_m8_r_tst <- predict(m8_glm_r, newdata = dummy_Grouped[-ix_train,], type='response')
MLmetrics::AUC(pred_m8_r_trn, dummy_PKDGroup$Target[ix_train]) # AUC 68.6 [100k]
MLmetrics::AUC(pred_m8_r_tst, dummy_PKDGroupGrouped$Target[-ix_train]) # AUC 68.6 [100k]
