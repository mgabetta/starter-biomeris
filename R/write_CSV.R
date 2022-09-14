##### Data Quality HN Core Data Registry 2.0 #####

# check ID algorithm ------ CHANGE THE ALGORITHM ID HERE
ID_alg <- paste0("TEST", format(Sys.Date(), "%y%m%d"),"_3")

# Read list of script previously run
file_scripts <- "/opt/redcap_dq/environment/logs/history.txt"
list_of_scripts <- readtext::readtext(file_scripts)

if (length(grep(ID_alg, list_of_scripts$text)) != 0) {
  message("You have already run this script")
  opt <- options(show.error.messages=FALSE)
  on.exit(options(opt))
  stop()
}

##### If not stopped run the code
# Add this script to the list of scripts previously run
sink(file_scripts, append = TRUE)
cat(ID_alg, file = file_scripts, sep = "\n", append = TRUE)
sink()

# load R packages
library(REDCapR)
library(dplyr)

#### FUNCTIONS ####

# prendo solo i form completi (per gli strumenti non ripetibili)
getComplete <- function(data, ps_pass, formStr){
  form_complete <- paste0(formStr, "_complete")
  data <- data %>% filter(a01_id %in% ps_pass$a01_id) 
  data[,form_complete] = ps_pass[ps_pass$redcap_repeat_instrument == "",form_complete] 

  for (i in 1:nrow(data)) {
    if (data[i,ncol(data)] != 1 | is.na(data[i,ncol(data)])){
      data[i,2:(ncol(data)-1)] = NA
    }
  }
  
  return(data)
  
}

# funzione per orizzontalizzazione
horizontal_data <- function(data_non_rep, data_rep){
  # orizzontalizzazione dei dati degli eventi ripetibili:
  # 1 - rinomino le variabili degli eventi ripetibili inserendo il numero di instance nel nome e metto tutto su una riga
  # 2 - match dei pazienti con dati evento base e porto tutti i dati di un paziente su un'unica riga
  data_final <- data.frame()
  for (i in 1:nrow(data_non_rep)) {
    data_id_hor <- data_non_rep[i,]
    data_id <- data_rep %>% subset(a01_id == data_non_rep$a01_id[i])
    if(!plyr::empty(data_id)){
      for (j in 1:nrow(data_id)) {
        instance <- data_id$redcap_repeat_instance[j]
        data_tmp <- data_id[j,] %>% select(-c(1:4))
        colnames(data_tmp) <- ifelse(grepl("\\d", colnames(data_tmp)), paste0(substring(colnames(data_tmp), first = 1, last = 1), "_",
                                                                              instance, "_",substring(colnames(data_tmp), first = 2)), 
                                     paste(instance, colnames(data_tmp), sep = "_"))
        # colnames(data_tmp) <- paste(colnames(data_tmp), instance, sep = "_")
        data_id_hor <- cbind(data_id_hor, data_tmp)
      }
    }
    data_final <- plyr::rbind.fill(data_final, data_id_hor)
  }
  return(data_final)
}

#### MAIN ####

# read properties file
setwd('/opt/redcap_dq/environment/scripts')
file_prop <- "../config/config.properties"
properties <- properties::read.properties(file_prop)

# analysis id --------- CHANGE THE ANALYSIS ID HERE
id_analysis <- "TEST"

# repeatable events -------- CHANGE THE REPEATABLE EVENTS TO INCLUDE IN YOUR ANALYSIS
# AE: adverse events
# IM: imaging
# TS: tumor specimen
rep_events <- data.frame("AE" = TRUE, "IM" = TRUE, "TS" = TRUE)

# read the most recent PS-PASS file
file_name <- list.files(path = '../data', pattern = paste0(id_analysis, "-pset-pass"))
ps_pass <- read.csv(paste0('../data/',tail(file_name, n = 1)))

# # estraggo dati da REDCap -------- strumenti non ripetibili
data_preliminary <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  forms_collapsed = "preliminary",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_demo <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "demographic_life_style",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_gensyndrome <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "previous_cancer_gensyndrome",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_cancer <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "cancer_under_study",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_stage <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "staging_procedures_and_stage",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_cancer_tr <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "cancer_under_study_treatment",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_progr1 <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "progressionrecurrencepersistent_disease",
  # forms_collapsed = "progressionrecurrencepersistent_disease_1",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_progr2 <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "progressionrecurrencepersistent_disease_a9bc",
  # forms_collapsed = "progressionrecurrencepersistent_disease_2",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_progr3 <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "progressionrecurrencepersistent_disease_1aa9",
  # forms_collapsed = "progressionrecurrencepersistent_disease_3",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_progr4 <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "progressionrecurrencepersistent_disease_d93f",
  # ùforms_collapsed = "progressionrecurrencepersistent_disease_4",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_progr5 <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "progressionrecurrencepersistent_disease_f74f",
  # forms_collapsed = "progressionrecurrencepersistent_disease_5",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_progr6 <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "progressionrecurrencepersistent_disease_3672",
  # forms_collapsed = "progressionrecurrencepersistent_disease_6",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_progr7 <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "progressionrecurrencepersistent_disease_1bf2",
  # forms_collapsed = "progressionrecurrencepersistent_disease_7",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_progr8 <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "progressionrecurrencepersistent_disease_aba2",
  # forms_collapsed = "progressionrecurrencepersistent_disease_8",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_progr9 <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "progressionrecurrencepersistent_disease_5f7c",
  # forms_collapsed = "progressionrecurrencepersistent_disease_9",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_progr10 <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "progressionrecurrencepersistent_disease_859b",
  # forms_collapsed = "progressionrecurrencepersistent_disease_10",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

data_fup <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "status_of_patient_at_fup",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

# estraggo dati da REDCap -------- strumenti ripetibili (Adverse events, Imaging available, Tumor specimen available )
if(rep_events$AE){
  data_ae <- redcap_read(
    redcap_uri = properties$uri, 
    token = properties$token,
    fields_collapsed = "a01_id,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance",
    forms_collapsed = "adverse_events",
    events_collapsed = "repeatable_arm_1")$data 
}

if(rep_events$IM){
  data_imaging <- redcap_read(
    redcap_uri = properties$uri, 
    token = properties$token,
    fields_collapsed = "a01_id,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance",
    forms_collapsed = "imaging_available",
    events_collapsed = "repeatable_arm_1")$data 
}
 
if(rep_events$TS){
  data_mat <- redcap_read(
    redcap_uri = properties$uri, 
    token = properties$token,
    fields_collapsed = "a01_id,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance",
    forms_collapsed = "tumor_specimen_available",
    events_collapsed = "repeatable_arm_1")$data
}

################################ SELECT FROM PS-PASS ################################ 
# get only instruments (non repeatable) with status = complete
data_preliminary <- getComplete(data_preliminary[,-ncol(data_preliminary)], ps_pass, "a07")
data_demo <- getComplete(data_demo[,-ncol(data_demo)], ps_pass, "b24")
data_gensyndrome <- getComplete(data_gensyndrome[,-ncol(data_gensyndrome)], ps_pass, "c28")
data_cancer <- getComplete(data_cancer[,-ncol(data_cancer)], ps_pass, "d34")
data_stage <- getComplete(data_stage[,-ncol(data_stage)], ps_pass, "e57")
data_cancer_tr <- getComplete(data_cancer_tr[,-ncol(data_cancer_tr)], ps_pass, "f107")
data_progr1 <- getComplete(data_progr1[,-ncol(data_progr1)], ps_pass, "g1_119")
data_progr2 <- getComplete(data_progr2[,-ncol(data_progr2)], ps_pass, "g2_119")
data_progr3 <- getComplete(data_progr3[,-ncol(data_progr3)], ps_pass, "g3_119")
data_progr4 <- getComplete(data_progr4[,-ncol(data_progr4)], ps_pass, "g4_119")
data_progr5 <- getComplete(data_progr5[,-ncol(data_progr5)], ps_pass, "g5_119")
data_progr6 <- getComplete(data_progr6[,-ncol(data_progr6)], ps_pass, "g6_119")
data_progr7 <- getComplete(data_progr7[,-ncol(data_progr7)], ps_pass, "g7_119")
data_progr8 <- getComplete(data_progr8[,-ncol(data_progr8)], ps_pass, "g8_119")
data_progr9 <- getComplete(data_progr9[,-ncol(data_progr9)], ps_pass, "g9_119")
data_progr10 <- getComplete(data_progr10[,-ncol(data_progr10)], ps_pass, "g10_119")
data_fup <- getComplete(data_fup[,-ncol(data_fup)], ps_pass, "h06")

data_non_rep <- cbind(data_preliminary, data_demo[,-1], data_gensyndrome[,-1], data_cancer[,-1],
                   data_stage[,-1], data_cancer_tr[,-1], data_progr1[,-1], data_progr2[,-1], data_progr3[,-1],
                   data_progr4[,-1], data_progr5[,-1], data_progr6[,-1], data_progr7[,-1], data_progr8[,-1],
                   data_progr9[,-1], data_progr10[,-1], data_fup[,-1])

# if TRUE get adverse event instruments with status = complete
if(rep_events$AE){
  data_ae <- data_ae %>% filter(!(is.na(prodlim::row.match(data_ae[,1:4], ps_pass[which(ps_pass$i07_complete == 1),1:4]))))
}

if(rep_events$IM){
  data_imaging <- data_imaging %>% filter(!(is.na(prodlim::row.match(data_imaging[,1:4], ps_pass[which(ps_pass$j06_complete == 1),1:4]))))
}

if(rep_events$TS){
  data_mat <- data_mat %>% filter(!(is.na(prodlim::row.match(data_mat[,1:4], ps_pass[which(ps_pass$k06_complete == 1),1:4]))))
}

################################ CSV ################################ 
# apply horizontalization function if TRUE
if(rep_events$AE){
  data_final <- horizontal_data(data_non_rep, data_ae)
} else {
  data_final <- data_non_rep
}

if(rep_events$IM){
  data_final <- horizontal_data(data_final, data_imaging)
}

if(rep_events$TS){
  data_final <- horizontal_data(data_final, data_mat)
}

# Get list of patients with core variables complete
ps_core <- ps_pass %>%
  filter(core == 1) %>%
  select("a01_id") %>%
  unique(.)

data_final <- data_final %>%
  mutate("site_id" = properties$id_centro) %>% 
  # add "age" variable
  mutate(age = ifelse(!is.na(d03_diagage_au), d03_diagage_au, d04_diagage_man)) %>%
  # add "tumor_site" variable
  mutate(site = ifelse(!is.na(d11_siterare), d11_siterare, d11_sitecomrar)) %>%
  # add "treatment" variable
  mutate(treatment = case_when(
    # SOLO UN TRATTAMENTO
    f01_surgery %in% c(1,2) & !(f53_radio %in% c(1,2)) & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(data_final), value = TRUE), inherits = TRUE) %in% c(1,2)) & f02_datesurg <= (d01_diagdate + 60) ~ "Only surgery",
    f53_radio %in% c(1,2) & !(f01_surgery %in% c(1,2)) & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(data_final), value = TRUE), inherits = TRUE) %in% c(1,2)) & f80_radiostartdate <= (d01_diagdate + 60) ~ "Only radiotherapy",
    f30_1_combofsystrt == 1 & !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(data_final), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) & f33_1_startdate_syst <= (d01_diagdate + 60) ~ "Only chemo",
    f30_1_combofsystrt == 2 & !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(data_final), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) & f33_1_startdate_syst <= (d01_diagdate + 60) ~ "Only immuno",
    f30_1_combofsystrt == 3 & !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(data_final), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) & f33_1_startdate_syst <= (d01_diagdate + 60) ~ "Only target",
    # SOLO CONCOMITANTI
    !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1 & f32_1_systsetting == 2 & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(data_final), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) & min(c(f33_1_startdate_syst, f80_radiostartdate)) <= (d01_diagdate + 60) &
      max(c(f33_1_startdate_syst, f80_radiostartdate)) <= min(c(f33_1_startdate_syst, f80_radiostartdate)) + 7 & max(c(f34_1_enddate_syst, f81_radioendate)) <= min(c(f34_1_enddate_syst, f81_radioendate)) + 7 ~ "Concomitant chemo/radio",
    # CHIRURGIA + altro
    f01_surgery %in% c(1,2) & f55_radiosett == 3 & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(data_final), value = TRUE), inherits = TRUE) %in% c(1,2)) & f02_datesurg <= (d01_diagdate + 60) & f80_radiostartdate <= (f02_datesurg + 75) ~ "Surgery + postoperative radiotherapy",
    f01_surgery %in% c(1,2) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 1 & f32_1_systsetting == 3 & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(data_final), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) & f02_datesurg <= (d01_diagdate + 60) & f33_1_startdate_syst <= (f02_datesurg + 60) ~ "Surgery + adjuvant chemo",
    f01_surgery %in% c(1,2) & f55_radiosett == 4 & f30_1_combofsystrt == 1 & f32_1_systsetting == 2 & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(data_final), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) & f02_datesurg <= (d01_diagdate + 60) &
      min(c(f33_1_startdate_syst, f80_radiostartdate)) <= (f02_datesurg + 75) & max(c(f33_1_startdate_syst, f80_radiostartdate)) <= min(c(f33_1_startdate_syst, f80_radiostartdate)) + 7 & max(c(f34_1_enddate_syst, f81_radioendate)) <= min(c(f34_1_enddate_syst, f81_radioendate)) + 7 ~ "Surgery + Postoperative radio concomitant to chemotherapy",
    # RADIO + altro
    !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1 & f32_1_systsetting == 3 & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(data_final), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) & f80_radiostartdate <= (d01_diagdate + 60) & f33_1_startdate_syst <= (f81_radioendate + 45) ~ "Radio + adjuvant chemo",
    !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1 & f32_1_systsetting == 2 & f30_2_combofsystrt == 1 & f32_2_systsetting == 3 & !(f29_3_systrt %in% c(1,2)) & min(c(f33_1_startdate_syst, f80_radiostartdate)) <= (d01_diagdate + 60) &
      max(c(f33_1_startdate_syst, f80_radiostartdate)) <= min(c(f33_1_startdate_syst, f80_radiostartdate)) + 7 & max(c(f34_1_enddate_syst, f81_radioendate)) <= min(c(f34_1_enddate_syst, f81_radioendate)) + 7 &
      f33_2_startdate_syst <= max(c(f33_1_startdate_syst, f80_radiostartdate)) + 45 ~ "Concomitant chemo/radio + adjuvant chemo",
    # CHEMO + altro
    !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 1 & f30_2_combofsystrt == 2 & !(f29_3_systrt %in% c(1,2)) &
      all(c(f33_1_startdate_syst, f33_2_startdate_syst) <= (d01_diagdate + 60)) ~ "Chemo + immuno",
    !(f01_surgery %in% c(1,2)) & f53_radio %in% c(1,2) & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 &
      !any(mget(grep("f29_[[:digit:]]_systrt", colnames(data_final), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) &
      f33_1_startdate_syst <= (d01_diagdate + 60) & f80_radiostartdate <= (f34_1_enddate_syst + 30) ~ "Neo-adjuvant chemo + radio",
    f01_surgery %in% c(1,2) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 &
      !any(mget(grep("f29_[[:digit:]]_systrt", colnames(data_final), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) &
      f33_1_startdate_syst <= (d01_diagdate + 60) & f02_datesurg <= (f34_1_enddate_syst + 30) ~ "Neo-adjuvant chemo + surgery",
    !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 & f30_2_combofsystrt == 1  & f32_2_systsetting == 2 & !(f29_3_systrt %in% c(1,2)) &
      f33_1_startdate_syst <= (d01_diagdate + 60) & min(c(f33_2_startdate_syst, f80_radiostartdate)) <= f34_1_enddate_syst + 30 &
      max(c(f33_2_startdate_syst, f80_radiostartdate)) <= min(c(f33_2_startdate_syst, f80_radiostartdate)) + 7 & max(c(f34_2_enddate_syst, f81_radioendate)) <= min(c(f34_2_enddate_syst, f81_radioendate)) + 7 ~ "Neo-adjuvant chemo + concomitant radio/chemo",
    !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 & f30_2_combofsystrt == 1  & f32_2_systsetting == 2 & f30_3_combofsystrt == 1 & f32_3_systsetting == 3 &
      f33_1_startdate_syst <= (d01_diagdate + 60) & min(c(f33_2_startdate_syst, f80_radiostartdate)) <= f34_1_enddate_syst + 30 &
      max(c(f33_2_startdate_syst, f80_radiostartdate)) <= min(c(f33_2_startdate_syst, f80_radiostartdate)) + 7 & max(c(f34_2_enddate_syst, f81_radioendate)) <= min(c(f34_2_enddate_syst, f81_radioendate)) + 7 &
      f33_3_startdate_syst <= max(c(f34_2_enddate_syst, f81_radioendate)) + 45 ~ "Neo-adjuvant chemo + concomitant radio/chemo + chemo adjuvant",
    !(f01_surgery %in% c(1,2)) & f55_radiosett == 5 & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 & f30_2_combofsystrt == 1  & f32_1_systsetting == 3 & !(f29_3_systrt %in% c(1,2)) &
      f33_1_startdate_syst <= (d01_diagdate + 60) & f80_radiostartdate <= f34_1_enddate_syst + 30 &
      f33_2_startdate_syst <= f81_radioendate + 45 ~ "Neo-adjuvant chemo + radio + chemo adjuvant",
    # IMMUNO + altro
    !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 2 & f30_2_combofsystrt == 3 & !(f29_3_systrt %in% c(1,2)) &
      all(c(f33_1_startdate_syst, f33_2_startdate_syst) <= (d01_diagdate + 60)) ~ "Immuno + target",
    !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 2 & f30_2_combofsystrt == 1 & !(f29_3_systrt %in% c(1,2)) &
      all(c(f33_1_startdate_syst, f33_2_startdate_syst) <= (d01_diagdate + 60)) ~ "Immuno + chemo",
    # SOLO CLINICAL TRIAL
    !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(data_final), value = TRUE), inherits = TRUE) %in% c(1,2)) & !(f99_othertrt %in% c(1,2)) & f105_clinicltrial == 1 ~ "Clinical trial only",
    # NON TRATTATO
    !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(data_final), value = TRUE), inherits = TRUE) %in% c(1,2)) & !(f99_othertrt %in% c(1,2)) ~ "No treatment",
    # OTHER
    TRUE ~ "Other")) %>%
  select(c("a01_id","hospital_id", "age", "site", "treatment"), everything()) %>%
  # inclusion criteria
  filter(a04_consent %in% c(1,3)) %>% #informed consent signed for clinical data
  filter(d03_diagage_au >= 18 | d04_diagage_man >= 18) %>% #age >= 18
  filter(as.integer(format(d01_diagdate, "%Y")) >= 2018) %>% #year of diagnosis >= 2018
  filter(a01_id %in% ps_core$a01_id) %>%  #core variables completed
  select(- grep("_complete", colnames(.), value = TRUE))

# write CSV 
file_name <- paste0(properties$id_centro,'-',id_analysis,'-DATA-csv-', format(Sys.Date(), "%y%m%d"), '.csv')
write.csv(data_final, file_name, na = "", row.names = F)

# Copy in /data
file.copy(from = paste0("/opt/redcap_dq/environment/scripts/", file_name), "/opt/redcap_dq/environment/data")

# Copy in /vantage6-starter_head_and_neck-user-vol/_data
file.copy(from = paste0("/opt/redcap_dq/environment/data/", file_name), "/var/lib/docker/volumes/vantage6-starter_head_and_neck-user-vol/_data")
file.rename(from = paste0("/var/lib/docker/volumes/vantage6-starter_head_and_neck-user-vol/_data/", file_name),
            to = "/var/lib/docker/volumes/vantage6-starter_head_and_neck-user-vol/_data/default.csv")

# Copy to /data
system("VTG_DATA_DIR=/data; 
       mkdir -p $VTG_DATA_DIR; 
       chmod -R 777 $VTG_DATA_DIR; 
       cp /var/lib/docker/volumes/vantage6-starter_head_and_neck-user-vol/_data/default.csv $VTG_DATA_DIR/default.csv;
       chmod 766 $VTG_DATA_DIR/default.csv")