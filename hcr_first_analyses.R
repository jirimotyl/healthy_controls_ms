library(tidyverse)
library(psych)
library(papaja)

#source csv file
hcr_data <- read_csv("final_data/hcr_export.csv", 
                na = "NA")
spec(hcr_data)

#copy basic demographic data into all timepoints
hcr_data_tidy <- 
  tibble(record_id = hcr_data$record_id, 
         birthday = hcr_data$birthday, 
         sex = hcr_data$sex,
         sex_other = hcr_data$sex_other,
         study___hcr = hcr_data$study___hcr,
         study___gauk = hcr_data$study___gauk,
         psych_normative_yes = hcr_data$psych_normative_yes,
         later_hand = hcr_data$later_hand,
         later_hand_other = hcr_data$later_hand_other,
         spu = hcr_data$spu,
         spu_type___1	= hcr_data$spu_type___1,
         spu_type___2	= hcr_data$spu_type___2,
         spu_type___3	= hcr_data$spu_type___3,
         spu_type___4	= hcr_data$spu_type___4,
         spu_type___5	= hcr_data$spu_type___5,
         spu_type___6	= hcr_data$spu_type___6,
         spu_type___7	= hcr_data$spu_type___7,
         spu_type___8	= hcr_data$spu_type___8,
         spu_type_note = hcr_data$spu_type_note,
         basic_demographic_data_complete = hcr_data$basic_demographic_data_complete,
  ) %>%
  filter(is.na(hcr_data$basic_demographic_data_complete)!=T) %>%
  left_join(hcr_data, by=c("record_id"), keep = F)

hcr_data_tidy <- select(hcr_data_tidy, 
                        -birthday.y, 
                        -sex.y,
                        -sex_other.y,
                        -study___hcr.y,
                        -study___gauk.y,
                        -psych_normative_yes.y,
                        -later_hand.y,
                        -later_hand_other.y,
                        -spu.y,
                        -spu_type___1.y,
                        -spu_type___2.y,
                        -spu_type___3.y,
                        -spu_type___4.y,
                        -spu_type___5.y,
                        -spu_type___6.y,
                        -spu_type___7.y,
                        -spu_type___8.y,
                        -spu_type_note.y,
                        -basic_demographic_data_complete.y)
hcr_data_tidy <- rename(hcr_data_tidy, 
                        birthday = birthday.x, 
                        sex = sex.x,
                        sex_other = sex_other.x,
                        study___hcr = study___hcr.x,
                        study___gauk = study___gauk.x,
                        psych_normative_yes = psych_normative_yes.x,
                        later_hand = later_hand.x,
                        later_hand_other = later_hand_other.x,
                        spu = spu.x,
                        spu_type___1	= spu_type___1.x,
                        spu_type___2	= spu_type___2.x,
                        spu_type___3	= spu_type___3.x,
                        spu_type___4	= spu_type___4.x,
                        spu_type___5	= spu_type___5.x,
                        spu_type___6	= spu_type___6.x,
                        spu_type___7	= spu_type___7.x,
                        spu_type___8	= spu_type___8.x,
                        spu_type_note = spu_type_note.x,
                        basic_demographic_data_complete = basic_demographic_data_complete.x)

hcr_data <- hcr_data_tidy
remove(hcr_data_tidy)

#add a few important calculated variables
hcr_data$age_mri <- as.numeric(NA)
hcr_data$age_mri <- as.numeric(lubridate::time_length(difftime(hcr_data$mri_datumvys, hcr_data$birthday, units = "days"),"years"))
hcr_data$age_psy <- as.numeric(NA)
hcr_data$age_psy <- as.numeric(lubridate::time_length(difftime(hcr_data$psycho_date, hcr_data$birthday, units = "days"),"years"))


#basic analyses of demographic data - psychology & rehabilitation
##select subsamples
###complete MRI
hcr_data_mri <- filter(hcr_data,
                       mri_complete == 2 | mri_complete == 1)
hcr_data_mri_m0 <- filter(hcr_data_mri,
                         redcap_event_name == "m0_arm_1")
hcr_data_mri_m12 <- filter(hcr_data_mri,
                          redcap_event_name == "m12_arm_1")
hcr_data_mri_m36 <- filter(hcr_data_mri,
                          redcap_event_name == "m36_arm_1")
hcr_data_mri_m48 <- filter(hcr_data_mri,
                          redcap_event_name == "m48_arm_1")
###complete psychology
hcr_data_psy <- filter(hcr_data,
                       psy_assessment_results_complete == 2 | psy_assessment_results_complete == 1)
hcr_data_psy_m0 <- filter(hcr_data_psy,
                          redcap_event_name == "m0_arm_1")
hcr_data_psy_m12 <- filter(hcr_data_psy,
                           redcap_event_name == "m12_arm_1")
hcr_data_psy_m36 <- filter(hcr_data_psy,
                           redcap_event_name == "m36_arm_1")
hcr_data_psy_m48 <- filter(hcr_data_psy,
                           redcap_event_name == "m48_arm_1")
###complete MRI & psychology
hcr_data_all <- filter(hcr_data,
                       (psy_assessment_results_complete == 2 | psy_assessment_results_complete == 1)&(mri_complete == 2 | mri_complete == 1))
hcr_data_all_m0 <- filter(hcr_data_psy,
                          redcap_event_name == "m0_arm_1")
hcr_data_all_m12 <- filter(hcr_data_psy,
                           redcap_event_name == "m12_arm_1")
hcr_data_all_m36 <- filter(hcr_data_psy,
                           redcap_event_name == "m36_arm_1")
hcr_data_all_m48 <- filter(hcr_data_psy,
                           redcap_event_name == "m48_arm_1")

##Demographics
hcr_data_all_grouped <- describeBy(hcr_data_all, group = "redcap_event_name", skew = T, IQR = T)
hcr_data_mri_grouped <- describeBy(hcr_data_mri, group = "redcap_event_name", skew = T, IQR = T)
hcr_data_psy_grouped <- describeBy(hcr_data_psy, group = "redcap_event_name", skew = T, IQR = T)

###supportive functions for tables
fn_2decimals <- function(x = Number) {
  papaja::printnum(x, digits = 2, big.interval=10)
}
fn_demographics_means <- function(Variable = "age_mri", Group = hcr_data_all_grouped$m0_arm_1){
  paste(fn_2decimals(Group[Variable,"mean"])," ","(\u00b1",fn_2decimals(Group[Variable,"sd"]),")", sep = "")
}
fn_demographics_n <- function(Group){
  as.character(Group["record_id","n"])
}
function_sex_count <- function(data, group) {
  event <- filter(data, redcap_event_name == group)
  no_females <- length(event$sex[event$sex=="F"])
  no_males <- length(event$sex[event$sex=="M"])
  paste(no_females, no_males, sep="/")
}

###number of cases in each group
hcr_tab_counts <- tribble(
  ~group, ~M0, ~M12, ~M36, ~M48,
  "MRI+PSY complete (N)", fn_demographics_n(hcr_data_all_grouped$m0_arm_1), fn_demographics_n(hcr_data_all_grouped$m12_arm_1), fn_demographics_n(hcr_data_all_grouped$m36_arm_1), fn_demographics_n(hcr_data_all_grouped$m48_arm_1),
  "PSY complete (N)", fn_demographics_n(hcr_data_psy_grouped$m0_arm_1), fn_demographics_n(hcr_data_psy_grouped$m12_arm_1), fn_demographics_n(hcr_data_psy_grouped$m36_arm_1), fn_demographics_n(hcr_data_psy_grouped$m48_arm_1),
  "MRI complete (N)", fn_demographics_n(hcr_data_mri_grouped$m0_arm_1), fn_demographics_n(hcr_data_mri_grouped$m12_arm_1), fn_demographics_n(hcr_data_mri_grouped$m36_arm_1), fn_demographics_n(hcr_data_mri_grouped$m48_arm_1)
)
write_excel_csv(hcr_tab_counts, "final_data/hcr_tab_group_counts.csv")

###basic demographics - MRI+PSY complete
hcr_tab_01a <- tribble(
  ~variable, ~M0, ~M12, ~M36, ~M48,
  "MRI+PSY complete (N)", fn_demographics_n(hcr_data_all_grouped$m0_arm_1), fn_demographics_n(hcr_data_all_grouped$m12_arm_1), fn_demographics_n(hcr_data_all_grouped$m36_arm_1), fn_demographics_n(hcr_data_all_grouped$m48_arm_1),
  "Age - MRI", fn_demographics_means("age_mri", hcr_data_all_grouped$m0_arm_1), fn_demographics_means("age_mri", hcr_data_all_grouped$m12_arm_1), fn_demographics_means("age_mri", hcr_data_all_grouped$m36_arm_1), fn_demographics_means("age_mri", hcr_data_all_grouped$m48_arm_1),
  "Age - MS Center", fn_demographics_means("age_psy", hcr_data_all_grouped$m0_arm_1), fn_demographics_means("age_psy", hcr_data_all_grouped$m12_arm_1), fn_demographics_means("age_psy", hcr_data_all_grouped$m36_arm_1), fn_demographics_means("age_psy", hcr_data_all_grouped$m48_arm_1),
  "N (Females/Males)", function_sex_count(hcr_data_all, "m0_arm_1"), function_sex_count(hcr_data_all, "m12_arm_1"), function_sex_count(hcr_data_all, "m36_arm_1"), function_sex_count(hcr_data_all, "m48_arm_1"),
  "Weight (kg)", fn_demographics_means("weight", hcr_data_all_grouped$m0_arm_1),"","","",
  "Height (cm)", fn_demographics_means("height", hcr_data_all_grouped$m0_arm_1),"","","",
  "25FWT", fn_demographics_means("twentyfive_foot_mean", hcr_data_all_grouped$m0_arm_1), fn_demographics_means("twentyfive_foot_mean", hcr_data_all_grouped$m12_arm_1), fn_demographics_means("twentyfive_foot_mean", hcr_data_all_grouped$m36_arm_1), fn_demographics_means("twentyfive_foot_mean", hcr_data_all_grouped$m48_arm_1),
  "9HPT Dominant Hand", fn_demographics_means("ninehp_dom_mean", hcr_data_all_grouped$m0_arm_1), fn_demographics_means("ninehp_dom_mean", hcr_data_all_grouped$m12_arm_1), fn_demographics_means("ninehp_dom_mean", hcr_data_all_grouped$m36_arm_1), fn_demographics_means("ninehp_dom_mean", hcr_data_all_grouped$m48_arm_1),
  "9HPT Non-Dominant Hand", fn_demographics_means("ninehp_nedom_mean", hcr_data_all_grouped$m0_arm_1), fn_demographics_means("ninehp_nedom_mean", hcr_data_all_grouped$m12_arm_1), fn_demographics_means("ninehp_nedom_mean", hcr_data_all_grouped$m36_arm_1), fn_demographics_means("ninehp_nedom_mean", hcr_data_all_grouped$m48_arm_1),
  "SDMT", fn_demographics_means("sdmt90_total", hcr_data_all_grouped$m0_arm_1), fn_demographics_means("sdmt90_total", hcr_data_all_grouped$m12_arm_1), fn_demographics_means("sdmt90_total", hcr_data_all_grouped$m36_arm_1), fn_demographics_means("sdmt90_total", hcr_data_all_grouped$m48_arm_1),
  "CVLT-II", fn_demographics_means("cvlt_total", hcr_data_all_grouped$m0_arm_1), fn_demographics_means("cvlt_total", hcr_data_all_grouped$m12_arm_1), fn_demographics_means("cvlt_total", hcr_data_all_grouped$m36_arm_1), fn_demographics_means("cvlt_total", hcr_data_all_grouped$m48_arm_1),
  "BVMT-R", fn_demographics_means("bvmt_total", hcr_data_all_grouped$m0_arm_1), fn_demographics_means("bvmt_total", hcr_data_all_grouped$m12_arm_1), fn_demographics_means("bvmt_total", hcr_data_all_grouped$m36_arm_1), fn_demographics_means("bvmt_total", hcr_data_all_grouped$m48_arm_1),
  "PASAT 3sec", fn_demographics_means("pasat_total", hcr_data_all_grouped$m0_arm_1), fn_demographics_means("pasat_total", hcr_data_all_grouped$m12_arm_1), fn_demographics_means("pasat_total", hcr_data_all_grouped$m36_arm_1), fn_demographics_means("pasat_total", hcr_data_all_grouped$m48_arm_1)
)
write_excel_csv(hcr_tab_01a, "final_data/hcr_tab_01a.csv")

###basic demographics - MRI+PSY complete
hcr_tab_01b <- tribble(
  ~variable, ~M0, ~M12, ~M36, ~M48,
  "MRI+PSY complete (N)", fn_demographics_n(hcr_data_psy_grouped$m0_arm_1), fn_demographics_n(hcr_data_psy_grouped$m12_arm_1), fn_demographics_n(hcr_data_psy_grouped$m36_arm_1), fn_demographics_n(hcr_data_psy_grouped$m48_arm_1),
  "Age - MRI", fn_demographics_means("age_mri", hcr_data_psy_grouped$m0_arm_1), fn_demographics_means("age_mri", hcr_data_psy_grouped$m12_arm_1), fn_demographics_means("age_mri", hcr_data_psy_grouped$m36_arm_1), fn_demographics_means("age_mri", hcr_data_psy_grouped$m48_arm_1),
  "Age - MS Center", fn_demographics_means("age_psy", hcr_data_psy_grouped$m0_arm_1), fn_demographics_means("age_psy", hcr_data_psy_grouped$m12_arm_1), fn_demographics_means("age_psy", hcr_data_psy_grouped$m36_arm_1), fn_demographics_means("age_psy", hcr_data_psy_grouped$m48_arm_1),
  "N (Females/Males)", function_sex_count(hcr_data_psy, "m0_arm_1"), function_sex_count(hcr_data_psy, "m12_arm_1"), function_sex_count(hcr_data_psy, "m36_arm_1"), function_sex_count(hcr_data_psy, "m48_arm_1"),
  "Weight (kg)", fn_demographics_means("weight", hcr_data_psy_grouped$m0_arm_1),"","","",
  "Height (cm)", fn_demographics_means("height", hcr_data_psy_grouped$m0_arm_1),"","","",
  "25FWT", fn_demographics_means("twentyfive_foot_mean", hcr_data_psy_grouped$m0_arm_1), fn_demographics_means("twentyfive_foot_mean", hcr_data_psy_grouped$m12_arm_1), fn_demographics_means("twentyfive_foot_mean", hcr_data_psy_grouped$m36_arm_1), fn_demographics_means("twentyfive_foot_mean", hcr_data_psy_grouped$m48_arm_1),
  "9HPT Dominant Hand", fn_demographics_means("ninehp_dom_mean", hcr_data_psy_grouped$m0_arm_1), fn_demographics_means("ninehp_dom_mean", hcr_data_psy_grouped$m12_arm_1), fn_demographics_means("ninehp_dom_mean", hcr_data_psy_grouped$m36_arm_1), fn_demographics_means("ninehp_dom_mean", hcr_data_psy_grouped$m48_arm_1),
  "9HPT Non-Dominant Hand", fn_demographics_means("ninehp_nedom_mean", hcr_data_psy_grouped$m0_arm_1), fn_demographics_means("ninehp_nedom_mean", hcr_data_psy_grouped$m12_arm_1), fn_demographics_means("ninehp_nedom_mean", hcr_data_psy_grouped$m36_arm_1), fn_demographics_means("ninehp_nedom_mean", hcr_data_psy_grouped$m48_arm_1),
  "SDMT", fn_demographics_means("sdmt90_total", hcr_data_psy_grouped$m0_arm_1), fn_demographics_means("sdmt90_total", hcr_data_psy_grouped$m12_arm_1), fn_demographics_means("sdmt90_total", hcr_data_psy_grouped$m36_arm_1), fn_demographics_means("sdmt90_total", hcr_data_psy_grouped$m48_arm_1),
  "CVLT-II", fn_demographics_means("cvlt_total", hcr_data_psy_grouped$m0_arm_1), fn_demographics_means("cvlt_total", hcr_data_psy_grouped$m12_arm_1), fn_demographics_means("cvlt_total", hcr_data_psy_grouped$m36_arm_1), fn_demographics_means("cvlt_total", hcr_data_psy_grouped$m48_arm_1),
  "BVMT-R", fn_demographics_means("bvmt_total", hcr_data_psy_grouped$m0_arm_1), fn_demographics_means("bvmt_total", hcr_data_psy_grouped$m12_arm_1), fn_demographics_means("bvmt_total", hcr_data_psy_grouped$m36_arm_1), fn_demographics_means("bvmt_total", hcr_data_psy_grouped$m48_arm_1),
  "PASAT 3sec", fn_demographics_means("pasat_total", hcr_data_psy_grouped$m0_arm_1), fn_demographics_means("pasat_total", hcr_data_psy_grouped$m12_arm_1), fn_demographics_means("pasat_total", hcr_data_psy_grouped$m36_arm_1), fn_demographics_means("pasat_total", hcr_data_psy_grouped$m48_arm_1)
)
write_excel_csv(hcr_tab_01b, "final_data/hcr_tab_01b.csv")


