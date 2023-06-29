##### Data Quality HN Core Data Registry 2.0 #####

# check ID algorithm ------ CHANGE THE ALGORITHM ID HERE
ID_alg <- paste0("TEST", format(Sys.Date(), "%y%m%d"),"_ICE")

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
rep_events <- data.frame("AE" = FALSE, "IM" = FALSE, "TS" = FALSE)

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

data_fup <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token,
  fields_collapsed = "a01_id",
  forms_collapsed = "status_of_patient_at_fup",
  events_collapsed = "non_repeatable_arm_1")$data %>%
  select(-c("redcap_event_name","redcap_repeat_instrument","redcap_repeat_instance"))

# estraggo dati da REDCap -------- strumenti ripetibili (Adverse events, Imaging available, Tumor specimen available)
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

# Bind non repeatable instruments
data_non_rep <- cbind(data_preliminary, data_demo[,-1], data_gensyndrome[,-1], data_cancer[,-1],
                   data_stage[,-1], data_cancer_tr[,-1], data_progr1[,-1], data_fup[,-1])

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

# Filter data final
data_final <- data_final %>%
  mutate(hospital_id = properties$id_centro) %>%
  # add "age" variable
  mutate(age = ifelse(!is.na(d03_diagage_au), d03_diagage_au, d04_diagage_man)) %>%
  # add "tumor_site" variable
  mutate(site = ifelse(!is.na(d11_siterare), d11_siterare, d11_sitecomrar)) %>%
  select(c("a01_id","hospital_id", "age", "site"), everything()) %>%
  select(- grep("_complete", colnames(.), value = TRUE))

# write CSV 
file_name <- paste0(properties$id_centro,'-',id_analysis,'-DATA-csv-', format(Sys.Date(), "%y%m%d"), '.csv')
write.csv(data_final, file_name, na = "", row.names = F)


# Copy in /opt/redcap_dq/environment/data
file.copy(from = paste0("/opt/redcap_dq/environment/scripts/", file_name), "/opt/redcap_dq/environment/data")

# Copy to /data
file.copy(from = paste0("/opt/redcap_dq/environment/data/", file_name), "/data")
file.rename(from = paste0("/data/", file_name), to = "/data/default.csv")
system("VTG_DATA_DIR=/data; 
       chmod 766 $VTG_DATA_DIR/default.csv")
       
# Copy in /vantage6-starter_head_and_neck-user-vol/_data
# system("container_name=\"vantage6-starter_head_and_neck-user\";
# datafile=\"/data/default.csv\";
# if [[ \"$( docker ps -q -f name=$container_name)\" && \"$( docker container inspect -f '{{.State.Status}}' $container_name )\" == \"running\" ]]; then
#       docker cp $datafile $container_name:/mnt/data
# fi;
# cp $datafile /data")

system("echo \'datafile=\"/data/default.csv\"; if [[ \"$( docker ps -q -f name=vantage6-starter_head_and_neck-user)\" && \"$( docker container inspect -f '{{.State.Status}}' vantage6-starter_head_and_neck-user )\" == \"running\" ]]; then docker cp $datafile vantage6-starter_head_and_neck-user:/mnt/data; fi' | bash")