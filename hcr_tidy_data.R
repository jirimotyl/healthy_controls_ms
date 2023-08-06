library(tidyverse)
library(haven)

#upload SPSS MRI data
data_prefinal_01 <- read_sav("Healthy_Jirka.sav")


##extract variable names to create the data fields in RedCap
data_variable_names <- select(
  data_prefinal_01,
  contains(".1"),
  -Timepoint.1
)

data_variable_names <- rename_with(data_variable_names, ~ gsub(".1", "", .x, fixed = TRUE))
data_variable_names_fin <- colnames(data_variable_names)
data_variable_names_tab <- as_tibble(data_variable_names_fin)
write_excel_csv(data_variable_names_tab, "variable_names.csv", na = "NA")

##select only structured data
data_prefinal_02 <- select(
  data_prefinal_01,
  -DatumVys,
  -PrijmeniJmeno,
  -Height,
  -Weight,
  -Sex,
  -Age_BL,
  -Year2,
  -Education,
  -APW_BL,
  -LRW_BL,
  -C3_vertebra_BL,
  -SCC_diameter_LR_BL,
  -"MUCCA_SV_cm2$_BL",
  -SCC_diameter_AP_BL,
  -"CCA_SV_cm2$_BL",
  -MUCCA_mm2_BL,
  -APW_M48,
  -LRW_M48,
  -C3_vertebra_M48,
  -SCC_diameter_AP_M48,
  -SCC_diameter_LR_M48,
  -VAR00005,
  -Date_NfL_sample,
  -NfL_pg_ml,
  -CV
)

data_prefinal_02 <- rename(data_prefinal_02,
                           record_id = Roche_ID,
                           mri_datumvys.1 = DatumVys.1,
                           mri_datumvys.2 = DatumVys.2,
                           mri_datumvys.3 = DatumVys.3,
                           mri_datumvys.4 = DatumVys.4,
                           mri_vekprivys.1= VekPriVys.1,
                           mri_vekprivys.2= VekPriVys.2,
                           mri_vekprivys.3= VekPriVys.3,
                           mri_vekprivys.4= VekPriVys.4)
data_prefinal_02$redcap_event_name.1 <- recode(data_prefinal_02$Timepoint.1,
                           "M0" = "m0_arm_1",
                           "M12" = "m12_arm_1",
                           "M24" = "m24_arm_1",
                           "M36" = "m36_arm_1",
                           "M48" = "m48_arm_1")
data_prefinal_02$redcap_event_name.1 <- na_if(data_prefinal_02$redcap_event_name.1, "")

data_prefinal_02$redcap_event_name.2 <- recode(data_prefinal_02$Timepoint.2,
                                       "M0" = "m0_arm_1",
                                       "M12" = "m12_arm_1",
                                       "M24" = "m24_arm_1",
                                       "M36" = "m36_arm_1",
                                       "M48" = "m48_arm_1"
                                       )
data_prefinal_02$redcap_event_name.2 <- na_if(data_prefinal_02$redcap_event_name.2, "")

data_prefinal_02$redcap_event_name.3 <- recode(data_prefinal_02$Timepoint.3,
                                       "M0" = "m0_arm_1",
                                       "M12" = "m12_arm_1",
                                       "M24" = "m24_arm_1",
                                       "M36" = "m36_arm_1",
                                       "M48" = "m48_arm_1")
data_prefinal_02$redcap_event_name.3 <- na_if(data_prefinal_02$redcap_event_name.3, "")

data_prefinal_02$redcap_event_name.4 <- recode(data_prefinal_02$Timepoint.4,
                                       "M0" = "m0_arm_1",
                                       "M12" = "m12_arm_1",
                                       "M24" = "m24_arm_1",
                                       "M36" = "m36_arm_1",
                                       "M48" = "m48_arm_1")
data_prefinal_02$redcap_event_name.4 <- na_if(data_prefinal_02$redcap_event_name.4, "")

data_prefinal_02$mri_complete.1 <- ifelse(!is.na(data_prefinal_02$mri_datumvys.1), 2, 0)
data_prefinal_02$mri_complete.2 <- ifelse(!is.na(data_prefinal_02$mri_datumvys.2), 2, 0)
data_prefinal_02$mri_complete.3 <- ifelse(!is.na(data_prefinal_02$mri_datumvys.3), 2, 0)
data_prefinal_02$mri_complete.4 <- ifelse(!is.na(data_prefinal_02$mri_datumvys.4), 2, 0)

data_prefinal_t1 <-  data_prefinal_02 %>% 
  select(
  record_id,
  contains(".1")
  ) %>%
  rename_with( ~ gsub(".1", "", .x, fixed = TRUE)) %>%
  rename_all(tolower) %>% 
  select(
    -timepoint,
    -year
  ) %>%
  filter(
    !is.na(redcap_event_name)
  )


data_prefinal_t2 <-  data_prefinal_02 %>% 
  select(
    record_id,
    contains(".2")
  ) %>%
  rename_with( ~ gsub(".2", "", .x, fixed = TRUE)) %>%
  rename_all(tolower) %>% 
  select(
    -timepoint,
    -year
  ) %>%
  filter(
    !is.na(redcap_event_name)
  )

data_prefinal_t3 <-  data_prefinal_02 %>% 
  select(
    record_id,
    contains(".3")
  ) %>%
  rename_with( ~ gsub(".3", "", .x, fixed = TRUE)) %>%
  rename_all(tolower) %>% 
  select(
    -timepoint,
    -year
  ) %>%
  filter(
    !is.na(redcap_event_name)
  ) %>%
  filter(
    record_id != 990029 ##filter out 2nd M12 timepoint of the record_id 990029
  )

data_prefinal_t4 <-  data_prefinal_02 %>% 
  select(
    record_id,
    contains(".4")
  ) %>%
  rename_with( ~ gsub(".4", "", .x, fixed = TRUE)) %>%
  rename_all(tolower) %>% 
  select(
    -timepoint,
    -year
  ) %>%
  filter(
    !is.na(redcap_event_name)
  )


write_excel_csv(data_prefinal_t1, "data_redcap_import_t1.csv")
write_excel_csv(data_prefinal_t2, "data_redcap_import_t2.csv")
write_excel_csv(data_prefinal_t3, "data_redcap_import_t3.csv")
write_excel_csv(data_prefinal_t4, "data_redcap_import_t4.csv")
