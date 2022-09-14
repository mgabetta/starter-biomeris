##### Data Quality HN Core Data Registry 2.0 #####

# check ID algorithm
ID_alg <- paste0("TEST", format(Sys.Date(), "%y%m%d"),"_2")

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
library(plyr)
library(dplyr)
library(openxlsx)

#### FUNCTIONS ####

## function that produces the df to write in the individual output file 
summary.rules <- function(data, method = 'count', decreasing = TRUE){
  ## METHOD:
  #  'count' : conteggio dei check FAIL per ogni regola 
  #  'patients' : lista di ogni regola (per cui c'? almeno un FAIL) e pazienti su ogni colonna (con una X se per quel paziente il check ? FAIL) 
  
  ## DECREASING:
  #  se TRUE, ordina le regole in ordine decrescente (da quella con pi? FAIL a quella con meno FAIL)
  
  check_rules <- plyr::count(data, c("category","description"))
  
  if(decreasing)
    check_rules <- check_rules %>% arrange(desc(freq))
  
  if(method == 'count'){
    check_rules <- check_rules %>%
      mutate(perc = formattable::percent(freq/nrow(data))) %>% # aggiungo percentuale sul totale degli errori
      bind_rows(dplyr::summarise(.,
                                 across(where(is.numeric), sum),
                                 across(where(is.character), ~"Total")))
  } else if (method == 'patients'){
    patients <- unique(data[,1]) # lista dei pazienti
    for(i in 1:length(patients)){
      checks <- c() # vettore dei check per ogni paziente
      for (j in 1:nrow(check_rules)) {
        # controllo se c'è la coppia paziente-check nel dataframe con i check FAIL: se c'è (paziente non supera il check) metto 1, altrimenti 0 (il paziente ha superato il check)
        checks[j] <- nrow(plyr::match_df(data[,c(1,3)], data.frame("a01_id" = patients[i], "description" = check_rules[j,2])))
      }
      check_rules[as.character(patients[i])] <- as.vector(matrix(checks)) #aggiungo al df colonna con check per ogni paziente 
    }
    # totali di colonna (totale per ogni paziente + totale check)
    check_rules <- check_rules %>%
      bind_rows(dplyr::summarise(.,
                                 across(where(is.numeric), sum),
                                 across(where(is.character), ~"Total")))
    # ordino pazienti in ordine decrescente di numero di errori e metto colonna freq in ultima posizione
    check_rules <- check_rules[,order(as.numeric(check_rules[nrow(check_rules),]), decreasing = T)] %>%
      select(c(category, description), everything()) %>%
      select(-freq, everything())
    # sostituisco 0 con NA
    check_rules[check_rules == 0] <- NA
  } else {
    stop("Please enter a valid method for this function")
  }
  return(check_rules)
}

## function to write the legend for each check 
getLegend <- function(data){
  legend <- data.frame()
  checks <- unique(data[c("description")])
  for (i in 1:nrow(checks)) {
    legend_check <- data[which(data$description == checks$description[i]),][1,-1] %>% select(grep("instance",colnames(.), invert = TRUE))
    legend <- rbind.fill(legend, legend_check)
  }
  return(legend)
}

## Translate branching logic to logical expression
renderLogic <- function(br_logic_str){
  expr_str <- br_logic_str %>% gsub("(\\[[[:graph:]]+\\])(<>\\'\\')", '!is.na(\\1)', .) %>%
    gsub("(\\[[[:graph:]]+\\])(=\\'\\')", 'is.na(\\1)', .) %>% 
    gsub("(\\[[[:graph:]]+\\])(<>\\\"\\\")", '!is.na(\\1)', .) %>%
    gsub("(\\[[[:graph:]]+\\])(=\\\"\\\")", 'is.na(\\1)', .) %>% 
    gsub(' OR | or ', ' | ', .) %>%
    gsub(' AND | and ', ' & ', .) %>%
    gsub('=', '==', .) %>%
    gsub('\\[', '', .) %>%
    gsub('\\]', '', .) %>%
    gsub('<>', '!=', .) %>%
    gsub("(\\()([[:digit:]]+)(\\))", "___\\2",.) %>% 
    gsub("rounddown", "floor", .) %>% 
    gsub("datediff", "difftime", .) %>%
    gsub("(difftime\\([[:graph:]]+\\,[[:graph:]]+)(\\,[[:graph:]]+\\,[[:graph:]]+)(\\)\\))","\\1\\3", .)
  return(expr_str)
}

## Get difference between dates in months
getDiffMonths <- function(start_date, end_date){
  if (end_date >= start_date){
    date_seq = seq(from = start_date, to = end_date, by = 'month')
    diff_months = length(date_seq) - 1
  } else {
    diff_months = -1
  }
  return(diff_months)
}

## calcolo missing e missing + unknown
getMissUnk <- function(ps, var, check_summary){
  check_summary <- rbind.fill(check_summary, data.frame("var" = var, "var_lab" = metadata$field_label[which(metadata$field_name == var)],
                                                        "form" = metadata$form_name[which(metadata$field_name == var)], "den" = nrow(ps),
                                                        "perc_miss" = nrow(filter(ps, is.na(get(var))))/nrow(ps),
                                                        "perc_miss_unk" = nrow(subset(ps, is.na(get(var)) | 
                                                                                        as.numeric(get(var)) == 999 | as.numeric(get(var)) == 9999))/nrow(ps)))
  return(check_summary)
}

## calcolo missing e missing + unknown checkbox
getMissUnkCheckbox <- function(ps_tmp, var_patt, check_summary){
  ps_tmp <- ps_tmp[, grep(pattern = var_patt, colnames(ps_tmp))] %>% cbind(., "sum" = rowSums(.))
  ind_unk <- grep(pattern = "999", colnames(ps_tmp))
  check_summary <- rbind.fill(check_summary, data.frame("var" = var_patt, "var_lab" = metadata$field_label[which(metadata$field_name == var_patt)],
                                                        "form" = metadata$form_name[which(metadata$field_name == var_patt)], "den" = nrow(ps_tmp),
                                                        "perc_miss" = nrow(filter(ps_tmp, sum == 0))/nrow(ps_tmp),
                                                        "perc_miss_unk" = ifelse(length(ind_unk) == 0,
                                                                                 nrow(filter(ps_tmp, sum == 0))/nrow(ps_tmp),
                                                                                 (nrow(filter(ps_tmp, sum == 0)) + sum(ps_tmp[,ind_unk]))/nrow(ps_tmp))))
  return(check_summary)
}

#### MAIN ####

# read properties file
setwd('/opt/redcap_dq/environment/scripts')
file_prop <- "../config/config.properties"
properties <- properties::read.properties(file_prop)

# analysis id
id_analysis <- "TEST"

# read PS-CTRL file
file_name <- list.files(path = '../data', pattern = paste0(id_analysis, "-pset-ctrl"))
# ps_ctrl <- read.csv(paste0('../data/', file_name))
ps_ctrl <- read.csv(paste0('../data/',tail(file_name, n = 1)))

# extract data from REDCap
redcap_data_new <- redcap_read_oneshot(
  redcap_uri = properties$uri,
  token = properties$token)$data

# extract metadata from REDCap
metadata <- redcap_metadata_read(
  redcap_uri = properties$uri,
  token = properties$token)$data

############################################## SELECT PS-CTRL ############################################## 
## New dataframe with new data (corrected) but old completeness status (from ps_ctrl)
# Exclude new repeatable events for old patients and all events for new patients
redcap_data <- redcap_data_new[prodlim::row.match(replace(ps_ctrl[,1:4],is.na(ps_ctrl[,1:4]), ""), replace(redcap_data_new[,1:4],is.na(redcap_data_new[,1:4]), "")),]

# Replace new completeness status with old completeness status
redcap_data[,c("a07_complete", "b24_complete","c28_complete", "d34_complete", "e57_complete", "f107_complete",
        grep("g[[:digit:]]+_119_complete", colnames(redcap_data), value = TRUE), "h06_complete", "i07_complete")] <- ps_ctrl[,c("a07_complete", "b24_complete","c28_complete", "d34_complete", "e57_complete", "f107_complete",
                                                                                                                                                        grep("g[[:digit:]]+_119_complete", colnames(ps_ctrl), value = TRUE), "h06_complete", "i07_complete")]

############################################## QC REPORT INDIVIDUAL ##############################################
### Rule #1 --------------------------------------------------------------------
# Cancer phase managed by the hospital (a06_phase): at least one flag has to be present

data_rule1 <- ddply(subset(redcap_data, a07_complete == 1), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Cancer phase managed by the hospital is not specified",
                    form1 = "Preliminary",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = gsub(" \\(.*?\\)","",gsub("<.*?>","",metadata$field_label[which(metadata$field_name == "a06_phase")])),
                    status = ifelse(a06_phase___1 != 0 | a06_phase___2 != 0 | a06_phase___3 != 0|
                                      a06_phase___4 != 0 | a06_phase___5 != 0 | a06_phase___6 != 0,'PASS','FAIL'))


### Rule #2 --------------------------------------------------------------------
# Date of birth (b01_birdate) prior (<) to date of first contact with the hospital (a05_date)

data_rule2 <- ddply(subset(redcap_data, b24_complete	 == 1 & a07_complete == 1 &
                             !is.na(b01_birdate) & !is.na(a05_date)), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Date of birth is not prior to first contact with the hospital",
                    form1 = "Demographic & life style",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata$field_label[which(metadata$field_name == "b01_birdate")],
                    value1 = b01_birdate,
                    form2 = "Preliminary",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "a05_date")],
                    value2 = ifelse(!is.na(a05_date_unk) & format(a05_date, "%d") == "15", a05_date - 14, a05_date),
                    status = ifelse(value1 < value2,'PASS','FAIL'))

### Rule #3 --------------------------------------------------------------------
# Pre treatment Weight (b16_weight) range: 30-200

data_rule3 <- ddply(subset(redcap_data, b24_complete == 1 & !(is.na(b16_weight)) & b16_weight != 999), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Pre treatment weight is out of range",
                    form1 = "Demographic & life style",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata$field_label[which(metadata$field_name == "b16_weight")],
                    value1 = b16_weight,
                    status = ifelse(value1 >= 30 & value1 <= 200,'PASS','FAIL'))

### Rule #4 --------------------------------------------------------------------
# Pre treatment Height (b17_height) range: 80-220

data_rule4 <- ddply(subset(redcap_data, b24_complete == 1 & !(is.na(b17_height)) & b17_height != 999), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Pre treatment height is out of range",
                    form1 = "Demographic & life style",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata$field_label[which(metadata$field_name == "b17_height")],
                    value1 = b17_height,
                    status = ifelse(value1 >= 80 & value1 <= 220,'PASS','FAIL'))

### Rule #5 --------------------------------------------------------------------
# Site of previous cancer (c02_prevcsite) not specified (if c01_prevc = yes)

data_rule5 <- ddply(subset(redcap_data, c28_complete == 1 & c01_prevc == 1), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Site of previous cancer is not specified",
                    form1 = "Previous cancer gen.syndrome",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata$field_label[which(metadata$field_name == "c02_prevcsite")],
                    value1 = c02_prevcsite,
                    status = ifelse(!(is.na(value1)),'PASS','FAIL'))

### Rule #6 --------------------------------------------------------------------
# Year of diagnosis (c03_prevcyear) not specified (if c01_prevc = yes)

data_rule6 <- ddply(subset(redcap_data, c28_complete	 == 1 & c01_prevc == 1), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Year of diagnosis is not specified",
                    form1 = "Previous cancer gen.syndrome",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata$field_label[which(metadata$field_name == "c03_prevcyear")],
                    value1 = c03_prevcyear,
                    status = ifelse(!(is.na(value1)),'PASS','FAIL'))

### Rule #7 --------------------------------------------------------------------
# Nello strumento  previous cancer genetic syndrome se c05_hnradioyear compilato  allora deve essere >= a  c03_prevcyear

data_rule7 <- ddply(subset(redcap_data, c28_complete == 1 & !(is.na(c05_hnradioyear)) & !(is.na(c03_prevcyear)) & !(c05_hnradioyear %in% c(999,9999)) & !(c03_prevcyear %in% c(999,9999))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Radiotherapy year is prior to previous cancer diagnosis",
                    form1 = "Previous cancer gen.syndrome",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata$field_label[which(metadata$field_name == "c05_hnradioyear")],
                    value1 = c05_hnradioyear,
                    form2 = "Previous cancer gen.syndrome",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "c03_prevcyear")],
                    value2 = c03_prevcyear,
                    status = ifelse(value1 >= value2,'PASS','FAIL'))

### Rule #8 --------------------------------------------------------------------
# Nello strumento  previous cancer genetic syndrome se c08_chemoyear compilato  allora deve essere >= a  c03_prevcyear

data_rule8 <- ddply(subset(redcap_data, c28_complete == 1 & !(is.na(c08_chemoyear)) & !(is.na(c03_prevcyear)) & !(c08_chemoyear %in% c(999,9999)) & !(c03_prevcyear %in% c(999,9999))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Chemotherapy year is prior to previous cancer diagnosis",
                    form1 = "Previous cancer gen.syndrome",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata$field_label[which(metadata$field_name == "c08_chemoyear")],
                    value1 = c08_chemoyear,
                    form2 = "Previous cancer gen.syndrome",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "c03_prevcyear")],
                    value2 = c03_prevcyear,
                    status = ifelse(value1 >= value2,'PASS','FAIL'))

### Rule #9 --------------------------------------------------------------------
# Nello strumento  previous cancer genetic syndrome se c09_targetyear compilato  allora deve essere >= a  c03_prevcyear

data_rule9 <- ddply(subset(redcap_data, c28_complete == 1 & !(is.na(c09_targetyear)) & !(is.na(c03_prevcyear)) & !(c09_targetyear %in% c(999,9999)) & !(c03_prevcyear %in% c(999,9999))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Target year is prior to previous cancer diagnosis",
                    form1 = "Previous cancer gen.syndrome",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata$field_label[which(metadata$field_name == "c09_targetyear")],
                    value1 = c09_targetyear,
                    form2 = "Previous cancer gen.syndrome",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "c03_prevcyear")],
                    value2 = c03_prevcyear,
                    status = ifelse(value1 >= value2,'PASS','FAIL'))

### Rule #10 --------------------------------------------------------------------
# Nello strumento  previous cancer genetic syndrome se c10_immyear compilato  allora deve essere >= a  c03_prevcyear

data_rule10 <- ddply(subset(redcap_data, c28_complete == 1 & !(is.na(c10_immyear)) & !(is.na(c03_prevcyear)) & !(c10_immyear %in% c(999,9999)) & !(c03_prevcyear %in% c(999,9999))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Immunotherapy year is prior to previous cancer diagnosis",
                     form1 = "Previous cancer gen.syndrome",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "c10_immyear")],
                     value1 = c10_immyear,
                     form2 = "Previous cancer gen.syndrome",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "c03_prevcyear")],
                     value2 = c03_prevcyear,
                     status = ifelse(value1 >= value2,'PASS','FAIL'))

### Rule #11 --------------------------------------------------------------------
# Nello strumento  previous cancer genetic syndrome se c12_surgyear compilato  allora deve essere >= a  c03_prevcyear

data_rule11 <- ddply(subset(redcap_data, c28_complete == 1 & !(is.na(c12_surgyear)) & !(is.na(c03_prevcyear)) & !(c12_surgyear %in% c(999,9999)) & !(c03_prevcyear %in% c(999,9999))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Immunotherapy year is prior to previous cancer diagnosis",
                     form1 = "Previous cancer gen.syndrome",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "c12_surgyear")],
                     value1 = c12_surgyear,
                     form2 = "Previous cancer gen.syndrome",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "c03_prevcyear")],
                     value2 = c03_prevcyear,
                     status = ifelse(value1 >= value2,'PASS','FAIL'))

### Rule #12 --------------------------------------------------------------------
# Date of diagnosis (d01_diagdate) posterior to Year of non-oncological surgery (c14_hnnosurgyear)

data_rule12 <- ddply(subset(redcap_data, d34_complete == 1 & c28_complete == 1 & !(is.na(d01_diagdate)) & !(is.na(c14_hnnosurgyear)) & !(c14_hnnosurgyear %in% c(999,9999))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Date of diagnosis is prior to year of previous head and neck non-oncological surgery",
                     form1 = "Cancer under study",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "d01_diagdate")],
                     value1 = format(d01_diagdate, "%Y"),
                     form2 = "Previous cancer gen.syndrome",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "c14_hnnosurgyear")],
                     value2 = c14_hnnosurgyear,
                     status = ifelse(value1 > value2,'PASS','FAIL'))

### Rule #13 --------------------------------------------------------------------
# Date of diagnosis (d01_diagdate) posterior to Year of oncological surgery (c12_surgyear)

data_rule13 <- ddply(subset(redcap_data, d34_complete == 1 & c28_complete == 1 & !(is.na(d01_diagdate)) & !(is.na(c12_surgyear)) & !(c12_surgyear %in% c(999,9999))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Date of diagnosis is prior to year of oncological surgery",
                     form1 = "Cancer under study",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "d01_diagdate")],
                     value1 = format(d01_diagdate, "%Y"),
                     form2 = "Previous cancer gen.syndrome",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "c12_surgyear")],
                     value2 = c12_surgyear,
                     status = ifelse(value1 > value2,'PASS','FAIL'))

### Rule #14 --------------------------------------------------------------------
# Date of diagnosis (d01_diagdate) posterior to Year of previous cancer (c03_prevcyear)

data_rule14 <- ddply(subset(redcap_data, d34_complete == 1 & c28_complete == 1 & !(is.na(d01_diagdate)) & !(is.na(c03_prevcyear)) & !(c03_prevcyear %in% c(999,9999))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Date of diagnosis is prior to year of previous cancer",
                     form1 = "Cancer under study",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "d01_diagdate")],
                     value1 = format(d01_diagdate, "%Y"),
                     form2 = "Previous cancer gen.syndrome",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "c03_prevcyear")],
                     value2 = c03_prevcyear,
                     status = ifelse(value1 > value2,'PASS','FAIL'))

### Rule #15 --------------------------------------------------------------------
# Date of diagnosis (d01_diagdate) posterior to Year of radiotherapy in head and neck for other non-oncological diseases (c16_hnradioyear)

data_rule15 <- ddply(subset(redcap_data, d34_complete == 1 & c28_complete == 1 & !(is.na(d01_diagdate)) & !(is.na(c16_hnradioyear)) & !(c16_hnradioyear %in% c(999,9999))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Date of diagnosis is prior to year of radiotherapy in head and neck for other non-oncological diseases",
                     form1 = "Cancer under study",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "d01_diagdate")],
                     value1 = format(d01_diagdate, "%Y"),
                     form2 = "Previous cancer gen.syndrome",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "c16_hnradioyear")],
                     value2 = c16_hnradioyear,
                     status = ifelse(value1 > value2,'PASS','FAIL'))

### Rule #16 --------------------------------------------------------------------
# Date of diagnosis (d01_diagdate) posterior to Year of bone marrow transplant (c22_bmtyear)

data_rule16 <- ddply(subset(redcap_data, d34_complete == 1 & c28_complete == 1 & !(is.na(d01_diagdate)) & !(is.na(c22_bmtyear)) & !(c22_bmtyear %in% c(999,9999))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Date of diagnosis is prior to year of bone marrow transplant",
                     form1 = "Cancer under study",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "d01_diagdate")],
                     value1 = format(d01_diagdate, "%Y"),
                     form2 = "Previous cancer gen.syndrome",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "c22_bmtyear")],
                     value2 = c22_bmtyear,
                     status = ifelse(value1 > value2,'PASS','FAIL'))

### Rule #17 --------------------------------------------------------------------
# Age at diagnosis automatic (d03_diagage_au) >= 18 

data_rule17 <- ddply(subset(redcap_data, d34_complete == 1 & !(is.na(d03_diagage_au))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Age at diagnosis (automatic) is under 18",
                     form1 = "Cancer under study",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "d03_diagage_au")],
                     value1 = d03_diagage_au,
                     status = ifelse(value1 >= 18,'PASS','FAIL'))

### Rule #18 --------------------------------------------------------------------
# Age at diagnosis manual (d04_diagage_man) >= 18 

data_rule18 <- ddply(subset(redcap_data, d34_complete == 1 & !(is.na(d04_diagage_man))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Age at diagnosis (manual) is under 18",
                     form1 = "Cancer under study",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "d04_diagage_man")],
                     value1 = d04_diagage_man,
                     status = ifelse(value1 >= 18,'PASS','FAIL'))

### Rule #19 --------------------------------------------------------------------
# Age at diagnosis manual (d04_diagage_man) mandatory if b01_birdate or d01_diagdate is missing 

data_rule19 <- ddply(subset(redcap_data, d34_complete == 1 & b24_complete == 1 & (is.na(b01_birdate) | is.na(d01_diagdate))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Age at diagnosis (manual) is not specified",
                     form1 = "Cancer under study",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "d04_diagage_man")],
                     value1 = d04_diagage_man,
                     status = ifelse(!(is.na(value1)),'PASS','FAIL'))

### Rule #20 --------------------------------------------------------------------
# If histology=9 and "site rare and common"=hypopharynx ; oropharynx; larynx;  lip" is an error

data_rule20 <- ddply(subset(redcap_data, d34_complete == 1 & d05_histo == 9 & !(is.na(d11_sitecomrar))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Site common and rare (AJCC) is not consistent with histology",
                     form1 = "Cancer under study",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "d11_sitecomrar")],
                     value1 = d11_sitecomrar,
                     form2 = "Cancer under study",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "d05_histo")],
                     value2 = d05_histo,
                     status = ifelse(value1 %in% c(7,8,9,11), 'FAIL','PASS'))

### Rule #21 --------------------------------------------------------------------
# If Imaging on the tumor (e02_im_primary) is yes , at least one imaging or unknown should be flagged for the primary site

data_rule21 <- ddply(subset(redcap_data, e57_complete == 1 & e02_im_primary == 1), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Imaging for primary site is not specified",
                     form1 = "Staging procedures and stage",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "e02_im_primary")],
                     value1 = e02_im_primary,
                     status = ifelse((!is.na(e03_ct) & e03_ct %in% c(1,2)) | 
                                       (!is.na(e04_mri) & e04_mri %in% c(1,2)) |
                                       (!is.na(e05_us) & e05_us == 1) | 
                                       (!is.na(e06_fdgpet) & e06_fdgpet == 1) | 
                                       (!is.na(e07_pet) & e07_pet == 1) |
                                       (!is.na(e08_opticdig) & e08_opticdig == 1) |
                                       (!is.na(e09_ctpet) & e09_ctpet == 1) |
                                       (!is.na(e10_unkim) & e10_unkim == 1) |
                                       !is.na(e11_otherim),
                                     'PASS', 'FAIL'))

### Rule #22 --------------------------------------------------------------------
# If Imaging for neck (e12_im_neck) is yes , at least one imaging or unknown should be flagged

data_rule22 <- ddply(subset(redcap_data, e57_complete == 1 & e12_im_neck == 1), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Imaging for neck is not specified",
                     form1 = "Staging procedures and stage",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "e12_im_neck")],
                     value1 = e12_im_neck,
                     status = ifelse((!is.na(e13_ct_neck) & e13_ct_neck %in% c(1,2)) | 
                                       (!is.na(e14_mri_neck) & e14_mri_neck %in% c(1,2)) |
                                       (!is.na(e15_us_neck) & e15_us_neck == 1) | 
                                       (!is.na(e16_fdgpet_neck) & e16_fdgpet_neck == 1) | 
                                       (!is.na(e17_pet_neck) & e17_pet_neck == 1) |
                                       (!is.na(e18_ctpet_neck) & e18_ctpet_neck == 1) |
                                       (!is.na(e19_unkim_neck) & e19_unkim_neck == 1) |
                                       !is.na(e20_otherim_neck),
                                     'PASS', 'FAIL'))

### Rule #23 --------------------------------------------------------------------
# If Imaging for metastasis (e21_im_m) is yes , at least one imaging or unknown should be flagged

data_rule23 <- ddply(subset(redcap_data, e57_complete == 1 & e21_im_m == 1), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Imaging for metastasis is not specified",
                     form1 = "Staging procedures and stage",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "e21_im_m")],
                     value1 = e21_im_m,
                     status = ifelse((!is.na(e22_ct_m) & e22_ct_m %in% c(1,2)) | 
                                       (!is.na(e23_mri_m) & e23_mri_m %in% c(1,2)) |
                                       (!is.na(e24_us_m) & e24_us_m == 1) | 
                                       (!is.na(e25_fdgpet_m) & e25_fdgpet_m == 1) | 
                                       (!is.na(e26_pet_m) & e26_pet_m == 1) |
                                       (!is.na(e27_ctpet_m) & e27_ctpet_m == 1) |
                                       (!is.na(e28_unkim_m) & e28_unkim_m == 1) |
                                       !is.na(e29_otherim_m),
                                     'PASS', 'FAIL'))

### Rule #24 --------------------------------------------------------------------
# # coherence between cT, cN and cM and clinical staging group when all competed

data_rule24 <- ddply(subset(redcap_data, e57_complete == 1 & d34_complete == 1 & !(is.na(e30_ct)) & !(is.na(e31_cn)) & !(is.na(e33_cm)) & !(is.na(e34_cstage))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "cT, cN and cM and clinical staging values are not consistent",
                     form1 = "Staging procedures and stage",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "e34_cstage")],
                     value1 = e34_cstage,
                     status = case_when(
                       # Major salivary gland, Nasal cavity and paranasal sinuses, Larinx, Lip, Oropharynx - p16 Negative and Hypopharynx
                       any(c(d11_siterare, d11_sitecomrar) %in% c(3,4,5)) | any(c(d11_siterare, d11_sitecomrar) == 1) | d11_sitecomrar %in% c(9,11,7) | (d11_sitecomrar == 8 & d27_p16 == 2) ~
                         case_when(
                           e30_ct == 2 & e31_cn == 2 & e33_cm == 1 ~ ifelse(e34_cstage == 1, 'PASS', 'FAIL'),
                           e30_ct == 4 & e31_cn == 2 & e33_cm == 1 ~ ifelse(e34_cstage == 2, 'PASS', 'FAIL'),
                           e30_ct == 5 & e31_cn == 2 & e33_cm == 1 ~ ifelse(e34_cstage == 3, 'PASS', 'FAIL'),
                           e30_ct == 6 & e31_cn == 2 & e33_cm == 1 ~ ifelse(e34_cstage == 4, 'PASS', 'FAIL'),
                           e30_ct %in% c(4,5,6) & e31_cn == 3 & e33_cm == 1 ~ ifelse(e34_cstage == 4, 'PASS', 'FAIL'),
                           e30_ct == 8 & e31_cn %in% c(2,3) & e33_cm == 1 ~ ifelse(e34_cstage == 6, 'PASS', 'FAIL'),
                           e30_ct %in% c(4,5,6,8) & e31_cn == 4 & e33_cm == 1 ~ ifelse(e34_cstage == 6, 'PASS', 'FAIL'),
                           e31_cn == 8 & e33_cm == 1 ~ ifelse(e34_cstage == 7, 'PASS', 'FAIL'),
                           e30_ct == 9 &  e33_cm == 1 ~ ifelse(e34_cstage == 7, 'PASS', 'FAIL'),
                           e33_cm == 2 ~ ifelse(e34_cstage == 8, 'PASS', 'FAIL') 
                         ),
                       # Nasopharynx
                       any(c(d11_siterare, d11_sitecomrar) == 2) ~
                         case_when(
                           e30_ct == 2 & e31_cn == 2 & e33_cm == 1 ~ ifelse(e34_cstage == 1, 'PASS', 'FAIL'),
                           e30_ct == 4 & e31_cn == 2 & e33_cm == 1 ~ ifelse(e34_cstage == 2, 'PASS', 'FAIL'),
                           e30_ct %in% c(3,4) & e31_cn == 3 & e33_cm == 1 ~ ifelse(e34_cstage == 3, 'PASS', 'FAIL'),
                           e30_ct == 5 & e31_cn %in% c(2,3) & e33_cm == 1 ~ ifelse(e34_cstage == 3, 'PASS', 'FAIL'),
                           e30_ct %in% c(3,4) & e31_cn == 4 & e33_cm == 1 ~ ifelse(e34_cstage == 4, 'PASS', 'FAIL'),
                           e30_ct == 5 & e31_cn == 4 & e33_cm == 1 ~ ifelse(e34_cstage == 4, 'PASS', 'FAIL'),
                           e30_ct == 6 & e31_cn %in% c(2,3,4) & e33_cm == 1 ~ ifelse(e34_cstage == 4, 'PASS', 'FAIL'),
                           e30_ct == 7 & e33_cm == 1 ~ ifelse(e34_cstage == 6, 'PASS', 'FAIL'),
                           e31_cn == 8 & e33_cm == 1 ~ ifelse(e34_cstage == 6, 'PASS', 'FAIL'),
                           e33_cm == 2 ~ ifelse(e34_cstage == 7, 'PASS', 'FAIL') 
                         ),
                       # Oropharynx -p16 Positive
                       d11_sitecomrar == 8 & d27_p16 == 1 ~
                         case_when(
                           e30_ct == 2 & e31_cn == 2 & e33_cm == 1 ~ ifelse(e34_cstage == 1, 'PASS', 'FAIL'),
                           e30_ct %in% c(4,5) & e31_cn %in% c(2,3) & e33_cm == 1 ~ ifelse(e34_cstage == 2, 'PASS', 'FAIL'),
                           e30_ct %in% c(4,5) & e31_cn == 4 & e33_cm == 1 ~ ifelse(e34_cstage == 3, 'PASS', 'FAIL'),
                           e30_ct == 6 & e31_cn %in% c(2,3,4) & e33_cm == 1 ~ ifelse(e34_cstage == 3, 'PASS', 'FAIL'),
                           e30_ct %in% c(4,5,6) & e31_cn == 8 & e33_cm == 1 ~ ifelse(e34_cstage == 4, 'PASS', 'FAIL'),
                           e30_ct == 7 & e33_cm == 1 ~ ifelse(e34_cstage == 4, 'PASS', 'FAIL'),
                           e33_cm == 2 ~ ifelse(e34_cstage == 5, 'PASS', 'FAIL') 
                         )
                     )
)

### Rule #25 ------------------------------------------------------------------
# ALERT: Select at least one M site

data_rule25 <- ddply(subset(redcap_data, e57_complete == 1 & d34_complete == 1 & (e33_cm == 2 | e34_cstage == 8 | (any(c(d11_siterare, d11_sitecomrar) == 2) & e34_cstage == 7) | (d11_sitecomrar == 8 & d27_p16 == 1 & e34_cstage == 5))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Select at least one M site (Clinical staging)",
                     form1 = "Staging procedures and stage",
                     event1 = "NON REPEATABLE", 
                     instance1 = redcap_repeat_instance,
                     var1 = "M site",
                     value1 = NA,
                     status = ifelse(any(c(e35_msite___1, e35_msite___2, e35_msite___3, e35_msite___4, e35_msite___5, e35_msite___6, e35_msite___7, e35_msite___8, e35_msite___9, e35_msite___999) == 1) , "PASS", "FAIL"))

### Rule #26 --------------------------------------------------------------------
# Number of positive lymph nodes, right side (e46_numposlympr) <= Number of lymph nodes removed, right side (e45_numlympr)

data_rule26 <- ddply(subset(redcap_data, e57_complete == 1 & !(is.na(e46_numposlympr)) & !(is.na(e45_numlympr)) & e46_numposlympr != 999 & e45_numlympr != 999), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Number of positive lymph nodes (right side) is greater than those removed",
                     form1 = "Staging procedures and stage",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "e46_numposlympr")],
                     value1 = e46_numposlympr,
                     form2 = "Staging procedures and stage",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "e45_numlympr")],
                     value2 = e45_numlympr,
                     status = ifelse(value1 <= value2, 'PASS','FAIL'))

### Rule #27 --------------------------------------------------------------------
# Number of positive lymph nodes, left side (e49_numposlympl) <= Number of lymph nodes removed, left side (e48_numlympl)

data_rule27 <- ddply(subset(redcap_data, e57_complete == 1 & !(is.na(e49_numposlympl)) & !(is.na(e48_numlympl)) & e49_numposlympl != 999 & e48_numlympl != 999), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Number of positive lymph nodes (left side) is greater than those removed",
                     form1 = "Staging procedures and stage",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "e49_numposlympl")],
                     value1 = e49_numposlympl,
                     form2 = "Staging procedures and stage",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "e48_numlympl")],
                     value2 = e48_numlympl,
                     status = ifelse(value1 <= value2, 'PASS','FAIL'))

### Rule #28 --------------------------------------------------------------------
# if neck dissection=yes at least one between e47 and e50 should be calculated

data_rule28 <- ddply(subset(redcap_data, e57_complete == 1 & e44_neckdiss == 1), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Neither LNR Right nor LNR Left is specified",
                     form1 = "Staging procedures and stage",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "e44_neckdiss")],
                     value1 = e44_neckdiss,
                     status = ifelse(is.na(e47_lnrr) & is.na(e50_lnrl), 'FAIL','PASS'))

### Rule #29 --------------------------------------------------------------------
# coherence between pT, pN and pM and pathological staging group when all competed.

data_rule29 <- ddply(subset(redcap_data, e57_complete == 1 & d34_complete == 1 & !(is.na(e37_pt)) & !(is.na(e38_pn)) & !(is.na(e53_pm)) & !(is.na(e54_pstage)) & e54_pstage != 999), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "pT, pN and pM and pathological staging values are not consistent",
                     form1 = "Staging procedures and stage",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "e54_pstage")],
                     value1 = e54_pstage,
                     status = case_when(
                       # Major salivary gland, Nasal cavity and paranasal sinuses, Larinx, Lip, Oropharynx - p16 Negative and Hypopharynx
                       any(c(d11_siterare, d11_sitecomrar) %in% c(3,4,5)) | any(c(d11_siterare, d11_sitecomrar) == 1) | d11_sitecomrar %in% c(9,11,7) | (d11_sitecomrar == 8 & d27_p16 == 2) ~
                         case_when(
                           e37_pt == 2 & e38_pn == 2 & e53_pm == 1 ~ ifelse(e54_pstage == 1, 'PASS', 'FAIL'),
                           e37_pt == 4 & e38_pn == 2 & e53_pm == 1 ~ ifelse(e54_pstage == 2, 'PASS', 'FAIL'),
                           e37_pt == 5 & e38_pn == 2 & e53_pm == 1 ~ ifelse(e54_pstage == 3, 'PASS', 'FAIL'),
                           e37_pt == 6 & e38_pn == 2 & e53_pm == 1 ~ ifelse(e54_pstage == 4, 'PASS', 'FAIL'),
                           e37_pt %in% c(4,5,6) & e38_pn == 3 & e53_pm == 1 ~ ifelse(e54_pstage == 4, 'PASS', 'FAIL'),
                           e37_pt == 8 & e38_pn %in% c(2,3) & e53_pm == 1 ~ ifelse(e54_pstage == 6, 'PASS', 'FAIL'),
                           e37_pt %in% c(4,5,6,8) & e38_pn == 4 & e53_pm == 1 ~ ifelse(e54_pstage == 6, 'PASS', 'FAIL'),
                           e38_pn == 8 & e53_pm == 1 ~ ifelse(e54_pstage == 7, 'PASS', 'FAIL'),
                           e37_pt == 9 &  e53_pm == 1 ~ ifelse(e54_pstage == 7, 'PASS', 'FAIL'),
                           e53_pm == 2 ~ ifelse(e54_pstage == 8, 'PASS', 'FAIL') 
                         ),
                       # Nasopharynx
                       any(c(d11_siterare, d11_sitecomrar) == 2) ~
                         case_when(
                           e37_pt == 2 & e38_pn == 2 & e53_pm == 1 ~ ifelse(e54_pstage == 1, 'PASS', 'FAIL'),
                           e37_pt == 4 & e38_pn == 2 & e53_pm == 1 ~ ifelse(e54_pstage == 2, 'PASS', 'FAIL'),
                           e37_pt %in% c(3,4) & e38_pn == 3 & e53_pm == 1 ~ ifelse(e54_pstage == 3, 'PASS', 'FAIL'),
                           e37_pt == 5 & e38_pn %in% c(2,3) & e53_pm == 1 ~ ifelse(e54_pstage == 3, 'PASS', 'FAIL'),
                           e37_pt %in% c(3,4) & e38_pn == 4 & e53_pm == 1 ~ ifelse(e54_pstage == 4, 'PASS', 'FAIL'),
                           e37_pt == 5 & e38_pn == 4 & e53_pm == 1 ~ ifelse(e54_pstage == 4, 'PASS', 'FAIL'),
                           e37_pt == 6 & e38_pn %in% c(2,3,4) & e53_pm == 1 ~ ifelse(e54_pstage == 4, 'PASS', 'FAIL'),
                           e37_pt == 7 & e53_pm == 1 ~ ifelse(e54_pstage == 6, 'PASS', 'FAIL'),
                           e38_pn == 8 & e53_pm == 1 ~ ifelse(e54_pstage == 6, 'PASS', 'FAIL'),
                           e53_pm == 2 ~ ifelse(e54_pstage == 7, 'PASS', 'FAIL') 
                         ),
                       # Oropharynx -p16 Positive
                       d11_sitecomrar == 8 & d27_p16 == 1 ~
                         case_when(
                           e37_pt == 2 & e38_pn == 2 & e53_pm == 1 ~ ifelse(e54_pstage == 1, 'PASS', 'FAIL'),
                           e37_pt %in% c(4,5) & e38_pn %in% c(2,3) & e53_pm == 1 ~ ifelse(e54_pstage == 2, 'PASS', 'FAIL'),
                           e37_pt %in% c(4,5) & e38_pn == 4 & e53_pm == 1 ~ ifelse(e54_pstage == 3, 'PASS', 'FAIL'),
                           e37_pt == 6 & e38_pn %in% c(2,3) & e53_pm == 1 ~ ifelse(e54_pstage == 3, 'PASS', 'FAIL'),
                           e37_pt %in% c(6,7) & e38_pn == 4 & e53_pm == 1 ~ ifelse(e54_pstage == 4, 'PASS', 'FAIL'),
                           e53_pm == 2 ~ ifelse(e54_pstage == 5, 'PASS', 'FAIL') 
                         )
                     )
)

### Rule #30 ------------------------------------------------------------------
# ALERT: Select at least one M site

data_rule30 <- ddply(subset(redcap_data, e57_complete == 1 & d34_complete == 1 & (e53_pm == 2 | e54_pstage == 8 | ((d11_siterare == 2 | d11_sitecomrar == 2) & e54_pstage == 7) | (d11_sitecomrar == 8 & d27_p16 == 1 & e54_pstage == 5))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Select at least one M site (Pathological staging)",
                     form1 = "Staging procedures and stage",
                     event1 = "NON REPEATABLE", 
                     instance1 = redcap_repeat_instance,
                     var1 = "M site",
                     value1 = NA,
                     status = ifelse(any(c(e55_pmsite___1, e55_pmsite___2, e55_pmsite___3, e55_pmsite___4, e55_pmsite___5, e55_pmsite___6, e55_pmsite___7, e55_pmsite___8, e55_pmsite___9, e55_pmsite___999) == 1), "PASS", "FAIL"))

### Rule #31 --------------------------------------------------------------------
# if surgery = yes, pathological stage (e54_pstage) must be filled in

data_rule31 <- ddply(subset(redcap_data, e57_complete == 1 & f107_complete == 1 & f01_surgery %in% c(1,2)), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "SURGERY PERFORMED: Pathological staging is not specified",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f01_surgery")],
                     value1 = f01_surgery,
                     form2 = "Staging procedures and stage",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "e54_pstage")],
                     value2 = e54_pstage,
                     status = ifelse(!(is.na(value2)), 'PASS','FAIL'))

### Rule #32 --------------------------------------------------------------------
# if surgery = yes, Margins after surgery (e41_margins) and Extranodal extension (e39_p_ene) must be filled in

data_rule32 <- ddply(subset(redcap_data, e57_complete == 1 & f107_complete == 1 & f01_surgery %in% c(1,2)), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "SURGERY PERFORMED: Extranodal extension and/or margins after surgery are not specified",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f01_surgery")],
                     value1 = f01_surgery,
                     form2 = "Staging procedures and stage",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "e39_p_ene")],
                     value2 = e39_p_ene,
                     form3 = "Staging procedures and stage",
                     event3 = "NON REPEATABLE",
                     instance3 = redcap_repeat_instance,
                     var3 = metadata$field_label[which(metadata$field_name == "e41_margins")],
                     value3 = e41_margins,
                     status = ifelse(!(is.na(value2)) & !(is.na(value3)), 'PASS','FAIL'))

### Rule #33 --------------------------------------------------------------------
# Date of surgery (f02_datesurg) >= (max 9 months) to date of diagnosis (d01_diagdate)

data_rule33 <- ddply(subset(redcap_data, f107_complete == 1 & d34_complete == 1 & !(is.na(f02_datesurg)) & !(is.na(d01_diagdate))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Date of surgery is not posterior (max 9 months) to date of diagnosis",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f02_datesurg")],
                     value1 = ifelse(!is.na(f02_datesurg_unk) & format(f02_datesurg, "%d") == "15", f02_datesurg + 15, f02_datesurg),
                     form2 = "Cancer under study",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "d01_diagdate")],
                     value2 = ifelse(!is.na(d01_diagdate_unk) & format(d01_diagdate, "%d") == "15", d01_diagdate - 14, d01_diagdate),
                     status = ifelse((value1 >= value2) & getDiffMonths(as.Date(value2, origin = "1970-01-01"), as.Date(value1, origin = "1970-01-01")) <= 9, 'PASS','FAIL'))

### Rule #34 --------------------------------------------------------------------
# Date of surgery (f02_datesurg) > to date of previous cancer (c12_surgyear)

data_rule34 <- ddply(subset(redcap_data, f107_complete == 1 & c28_complete == 1 & !(is.na(f02_datesurg)) & !(is.na(c12_surgyear)) & !(c12_surgyear %in% c(999,9999))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Date of surgery is not posterior to year of previous surgery",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f02_datesurg")],
                     value1 = format(f02_datesurg, "%Y"),
                     form2 = "Previous cancer gen.syndrome",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "c12_surgyear")],
                     value2 = c12_surgyear,
                     status = ifelse(value1 > value2,'PASS','FAIL'))

### Rule #35 --------------------------------------------------------------------
# Date of neck surgery (f12_datenecksurg) >= (max 9 months) to date of diagnosis (d01_diagdate)

data_rule35 <- ddply(subset(redcap_data, f107_complete == 1 & d34_complete == 1 & !(is.na(f12_datenecksurg)) & !(is.na(d01_diagdate))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Date of neck surgery is not posterior (max 9 months) to date of diagnosis",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f12_datenecksurg")],
                     value1 = ifelse(!is.na(f12_datenecksurg_unk) & format(f12_datenecksurg, "%d") == "15", f12_datenecksurg + 15, f12_datenecksurg),
                     form2 = "Cancer under study",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "d01_diagdate")],
                     value2 = ifelse(!is.na(d01_diagdate_unk) & format(d01_diagdate, "%d") == "15", d01_diagdate - 14, d01_diagdate),
                     status = ifelse((value1 >= value2)  & getDiffMonths(as.Date(value2, origin = "1970-01-01"), as.Date(value1, origin = "1970-01-01")) <= 9, 'PASS','FAIL'))
### Rule #36 --------------------------------------------------------------------
# Date of neck surgery (f12_datenecksurg) > to date of previous cancer (c12_surgyear)

data_rule36 <- ddply(subset(redcap_data, f107_complete == 1 & c28_complete == 1 & !(is.na(f12_datenecksurg)) & !(is.na(c12_surgyear)) & !(c12_surgyear %in% c(999,9999))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Date of neck surgery is not posterior to year of previous surgery",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f12_datenecksurg")],
                     value1 = format(f12_datenecksurg, "%Y"),
                     form2 = "Previous cancer gen.syndrome",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "c12_surgyear")],
                     value2 = c12_surgyear,
                     status = ifelse(value1 > value2,'PASS','FAIL'))

### Rule #37 --------------------------------------------------------------------
# Surgery on M yes if patient is metastatic

data_rule37 <- ddply(subset(redcap_data, f107_complete == 1 & e57_complete == 1 & d34_complete == 1 & f20_surgm == 1), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Surgery on M performed but clinical or pathological staging is not metastatic",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f20_surgm")],
                     value1 = f20_surgm,
                     status = ifelse(((!is.na(e34_cstage) & e34_cstage == 8) | (!is.na(e54_pstage) & e54_pstage == 8)) | (((!is.na(d11_siterare) & d11_siterare == 2) | (!is.na(d11_sitecomrar) & d11_sitecomrar == 2)) & ((!is.na(e34_cstage) & e34_cstage == 7) | (!is.na(e54_pstage) & e54_pstage == 7))) |
                                       ((!is.na(d11_sitecomrar) & d11_sitecomrar == 8) & d27_p16 == 1 & ((!is.na(e34_cstage) & e34_cstage == 5) | (!is.na(e54_pstage) & e54_pstage == 5))),
                                     'PASS','FAIL'))

### Rule #38 --------------------------------------------------------------------
# if surgery on M yes (f20_surgm) at least one of f21_sitesurgm must be flagged  

data_rule38 <- ddply(subset(redcap_data, f107_complete == 1 & f20_surgm == 1), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Surgery on M performed but site of surgery is not specified",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f20_surgm")],
                     value1 = f20_surgm,
                     status = ifelse(any(c(f21_sitesurgm___1, f21_sitesurgm___2, f21_sitesurgm___3, f21_sitesurgm___4, f21_sitesurgm___5,
                                           f21_sitesurgm___6, f21_sitesurgm___7, f21_sitesurgm___8, f21_sitesurgm___9, f21_sitesurgm___999) == 1),
                                     'PASS','FAIL'))

### Rule #39 --------------------------------------------------------------------
data_rule39 <- data.frame()

var_tmp <- grep("f21_sitesurgm___[[:digit:]]+", colnames(redcap_data), value = TRUE)

for (i in 1:length(var_tmp)) {
  data_tmp <- ddply(subset(redcap_data, e57_complete == 1 & f107_complete == 1 & get(var_tmp[i]) == 1), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Site of surgery on metastasis is not consistent with M site",
                    form1 = "Cancer Under Study Treatment",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata$field_label[which(metadata$field_name == "f21_sitesurgm")],
                    value1 = get(var_tmp[i]),
                    status = ifelse(get(sub("f21_sitesurgm", "e35_msite", var_tmp[i])) == 1 | get(sub("f21_sitesurgm", "e55_pmsite", var_tmp[i])) == 1, 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule39 <- rbind.fill(data_rule39, data_tmp)
  
}

### Rule #40 --------------------------------------------------------------------
# Date of surgery on M (f22_datesurgm) > date of diagnosis (d01_diagdate) (max 9 months)

data_rule40 <- ddply(subset(redcap_data, f107_complete == 1 & d34_complete == 1 & !(is.na(f22_datesurgm)) & !(is.na(d01_diagdate))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Date of surgery on M is not posterior (max 9 months) to date of diagnosis",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f22_datesurgm")],
                     value1 = ifelse(!is.na(f22_datesurgm_unk) & format(f22_datesurgm, "%d") == "15", f22_datesurgm + 15, f22_datesurgm),
                     form2 = "Cancer under study",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "d01_diagdate")],
                     value2 = ifelse(!is.na(d01_diagdate_unk) & format(d01_diagdate, "%d") == "15", d01_diagdate - 14, d01_diagdate),
                     status = ifelse((value1 > value2)  & getDiffMonths(as.Date(value2, origin = "1970-01-01"), as.Date(value1, origin = "1970-01-01")) <= 9, 'PASS','FAIL'))


### Rule #41 --------------------------------------------------------------------
# Date of surgery on M (f22_datesurgm) > to date of previous cancer (c12_surgyear)

data_rule41 <- ddply(subset(redcap_data, f107_complete == 1 & c28_complete == 1 & !(is.na(f22_datesurgm)) & !(is.na(c12_surgyear)) & !(c12_surgyear %in% c(999,9999))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Date of surgery on M is not posterior to year of previous surgery",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f22_datesurgm")],
                     value1 = format(f22_datesurgm, "%Y"),
                     form2 = "Previous cancer gen.syndrome",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "c12_surgyear")],
                     value2 = c12_surgyear,
                     status = ifelse(value1 > value2,'PASS','FAIL'))

### Rule #42 --------------------------------------------------------------------
# Date of unplanned surgery (f26_dateunplsurg) - date of surgery 1 <= 90

data_rule42 <- ddply(subset(redcap_data, f107_complete == 1 & !(is.na(f26_dateunplsurg)) & !(is.na(f02_datesurg)) & f26_dateunplsurg > f02_datesurg), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Difference between date of unplanned surgery and date of first surgery is greater than 90 days",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f26_dateunplsurg")],
                     value1 = ifelse(!is.na(f26_dateunplsurg_unk) & format(f26_dateunplsurg, "%d") == "15", f26_dateunplsurg + 15, f26_dateunplsurg),
                     form2 = "Cancer Under Study Treatment",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "f02_datesurg")],
                     value2 = ifelse(!is.na(f02_datesurg_unk) & format(f02_datesurg, "%d") == "15", f02_datesurg - 14, f02_datesurg),
                     status = ifelse((value1 - value2) <= 90, 'PASS','FAIL'))

### Rule #43 --------------------------------------------------------------------
# Date of unplanned surgery (f26_dateunplsurg) > date of surgery 1

data_rule43 <- ddply(subset(redcap_data, f107_complete == 1 & !(is.na(f26_dateunplsurg)) & !(is.na(f02_datesurg))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Date of unplanned surgery is not posterior to date of first surgery",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f26_dateunplsurg")],
                     value1 = ifelse(!is.na(f26_dateunplsurg_unk) & format(f26_dateunplsurg, "%d") == "15", f26_dateunplsurg - 14, f26_dateunplsurg),
                     form2 = "Cancer Under Study Treatment",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "f02_datesurg")],
                     value2 = ifelse(!is.na(f02_datesurg_unk) & format(f02_datesurg, "%d") == "15", f02_datesurg + 15, f02_datesurg),
                     status = ifelse(value1 > value2, 'PASS','FAIL'))

### Rule #44 --------------------------------------------------------------------
# If systemic treatment alone, surgery and radiotherapy != yes
data_rule44 <- data.frame()

var_tmp <- grep("f32_[[:digit:]]_systsetting", colnames(redcap_data), value = TRUE)
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, f107_complete == 1 & get(metadata_tmp$field_name[i]) == 4), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Systemic treatment setting is alone but surgery or radiotherapy have been performed",
                    form1 = "Cancer Under Study Treatment",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = get(metadata_tmp$field_name[i]),
                    status = ifelse(!any(c(f01_surgery, f53_radio) %in% c(1,2)), 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule44 <- rbind.fill(data_rule44, data_tmp)
}

### Rule #45 --------------------------------------------------------------------
# If systemic treatment concomitamnt and radio is not concomitant it must exists another concomitant systemic treatment
data_rule45 <- data.frame()

var_tmp <- grep("f32_[[:digit:]]_systsetting", colnames(redcap_data), value = TRUE)
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, f107_complete == 1 & get(metadata_tmp$field_name[i]) == 2 & f55_radiosett %in% c(1,3,5)), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Systemic treatment is concomitant to another systemic tratment that is not specified",
                    form1 = "Cancer Under Study Treatment",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = get(metadata_tmp$field_name[i]),
                    status = ifelse(any(mget(grep("f32_[[:digit:]]_systsetting", colnames(redcap_data), value = TRUE)[-i], inherits = TRUE) == 2) & 
                                      !is.na(any(mget(grep("f32_[[:digit:]]_systsetting", colnames(redcap_data), value = TRUE)[-i], inherits = TRUE) == 2)), 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule45 <- rbind.fill(data_rule45, data_tmp)
}

### Rule #46 --------------------------------------------------------------------
# Start date systemic treatment (f33_1_startdate_syst) within 9 months from date of diagnosis (d01_diagdate)
data_rule46 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("f33_[[:digit:]]_startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, f107_complete == 1 & d34_complete == 1 & !(is.na(get(metadata_tmp$field_name[i]))) & !(is.na(d01_diagdate))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Start date of systemic treatment is not within 9 months from date of diagnosis",
                    form1 = "Cancer Under Study Treatment",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(!is.na(get(paste0("f33_", i, "_startdate_syst_unk"))) & format(get(metadata_tmp$field_name[i]), "%d") == "15", get(metadata_tmp$field_name[i]) + 15, get(metadata_tmp$field_name[i])),
                    form2 = "Cancer under study",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "d01_diagdate")],
                    value2 = ifelse(!is.na(d01_diagdate_unk) & format(d01_diagdate, "%d") == "15", d01_diagdate - 14, d01_diagdate),
                    status = ifelse(getDiffMonths(as.Date(value2, origin = "1970-01-01"), as.Date(value1, origin = "1970-01-01")) >= 0 &
                                      getDiffMonths(as.Date(value2, origin = "1970-01-01"), as.Date(value1, origin = "1970-01-01")) <= 9, 
                                    'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule46 <- rbind.fill(data_rule46, data_tmp)
}

### Rule #47 --------------------------------------------------------------------
# if neoadiuvant, date must be prior to surgery or radiotherapy
data_rule47 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("f33_[[:digit:]]_startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, f107_complete == 1 & get(paste0("f32_", i, "_systsetting")) == 1 & !(is.na(get(metadata_tmp$field_name[i]))) & (!(is.na(f02_datesurg)) | !(is.na(f80_radiostartdate)))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Neo-adjuvant systemic treatment: start date is not prior to surgery or radiotherapy",
                    form1 = "Cancer Under Study Treatment",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(!is.na(get(paste0("f33_", i, "_startdate_syst_unk"))) & format(get(metadata_tmp$field_name[i]), "%d") == "15", get(metadata_tmp$field_name[i]) + 15, get(metadata_tmp$field_name[i])),
                    form2 = "Cancer Under Study Treatment",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = ifelse(is.na(f02_datesurg), metadata$field_label[which(metadata$field_name == "f80_radiostartdate")], metadata$field_label[which(metadata$field_name == "f02_datesurg")]),
                    value2 = ifelse(is.na(f02_datesurg), f80_radiostartdate, f02_datesurg),
                    status = ifelse(value1 < value2,'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule47 <- rbind.fill(data_rule47, data_tmp)
}

### Rule #48 --------------------------------------------------------------------
# if neoadiuvant, date must be prior to surgery or radiotherapy
data_rule48 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("f33_[[:digit:]]_startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, f107_complete == 1 & get(paste0("f32_", i, "_systsetting")) == 3 & !(is.na(get(metadata_tmp$field_name[i]))) & (!(is.na(f02_datesurg)) | !(is.na(f81_radioendate)))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Adjuvant systemic treatment: start date is not posterior to surgery or radiotherapy",
                    form1 = "Cancer Under Study Treatment",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(!is.na(get(paste0("f33_", i, "_startdate_syst_unk"))) & format(get(metadata_tmp$field_name[i]), "%d") == "15", get(metadata_tmp$field_name[i]) - 14, get(metadata_tmp$field_name[i])),
                    form2 = "Cancer Under Study Treatment",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = ifelse(is.na(f02_datesurg), metadata$field_label[which(metadata$field_name == "f81_radioendate")], metadata$field_label[which(metadata$field_name == "f02_datesurg")]),
                    value2 = ifelse(is.na(f02_datesurg), f81_radioendate, f02_datesurg),
                    status = ifelse(value1 > value2,'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule48 <- rbind.fill(data_rule48, data_tmp)
}

### Rule #49 --------------------------------------------------------------------
# if concomitant, date should be within the start and end date of radiotherapy (chemo must start 7 gg prior start radio or end 7 gg post end radio)
data_rule49 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("f33_[[:digit:]]_startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)


for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, f107_complete == 1 & get(paste0("f32_", i, "_systsetting")) == 2 & !(is.na(get(metadata_tmp$field_name[i]))) & f55_radiosett %in% c(2,4,6)), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Concomitant systemic treatment: start date is not within radiotherapy start and end date",
                    form1 = "Cancer Under Study Treatment",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(!is.na(get(paste0("f33_", i, "_startdate_syst_unk"))) & format(get(metadata_tmp$field_name[i]), "%d") == "15", get(metadata_tmp$field_name[i]) - 14, get(metadata_tmp$field_name[i])),
                    form2 = "Cancer Under Study Treatment",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "f80_radiostartdate")],
                    value2 = ifelse(!is.na(f80_radiostartdate_unk) & format(f80_radiostartdate, "%d") == "15", f80_radiostartdate + 15, f80_radiostartdate),
                    form3 = "Cancer Under Study Treatment",
                    event3 = "NON REPEATABLE",
                    instance3 = redcap_repeat_instance,
                    var3 = metadata$field_label[which(metadata$field_name == "f81_radioendate")],
                    value3 = ifelse(!is.na(f81_radioendate_unk) & format(f81_radioendate, "%d") == "15", f81_radioendate - 14, f81_radioendate),
                    status = ifelse(value1 >= ifelse(get(paste0("f30_", i, "_combofsystrt")) == 1, value2 - 7, value2) & value1 <= ifelse(get(paste0("f30_", i, "_combofsystrt")) == 1, value3 + 7, value3),'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule49 <- rbind.fill(data_rule49, data_tmp)
}

### Rule #50 -----------------------------------------------------------------
# if concomitant to another systemic treatment must exist a systemic treatment and must start the same day
data_rule50 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("f33_[[:digit:]]_startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:(nrow(metadata_tmp) - 1)) {
  data_tmp <- ddply(subset(redcap_data, f107_complete == 1 & get(paste0("f32_", i, "_systsetting")) == 2 & !(is.na(get(metadata_tmp$field_name[i]))) & !(f55_radiosett %in% c(2,4,6))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Start dates of concomitant systemic treatments are not the same",
                    form1 = "Cancer Under Study Treatment",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = get(metadata_tmp$field_name[i]),
                    form2 = "Cancer Under Study Treatment",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata_tmp$field_label[i],
                    value2 = ifelse(i == 1 & f32_2_systsetting == 2, f33_2_startdate_syst, f33_3_startdate_syst),
                    status = ifelse(value1 == value2,'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule50 <- rbind.fill(data_rule50, data_tmp)
}

### Rule #51 --------------------------------------------------------------------
# Start date systemic treatment posterior to year of systemic treatment due to previous cancer
data_rule51 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("f33_[[:digit:]]_startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, f107_complete == 1 & c28_complete == 1 & !(is.na(get(metadata_tmp$field_name[i]))) & c06_syst == 1), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Start date of systemic treatment is not posterior to date of previous systemic treatment",
                    form1 = "Cancer Under Study Treatment",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(i == 1, format(get(metadata_tmp$field_name[i]), "%Y"), get(metadata_tmp$field_name[i])),
                    form2 = ifelse(i == 1, "Previous cancer gen.syndrome", "Cancer Under Study Treatment"),
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = ifelse(i == 1, case_when(c07_systtype___1 == 1 ~ metadata$field_label[which(metadata$field_name == "c08_chemoyear")],
                                                    c07_systtype___2 == 1 ~ metadata$field_label[which(metadata$field_name == "c09_targetyear")],
                                                    c07_systtype___3 == 1 ~ metadata$field_label[which(metadata$field_name == "c10_immyear")]),
                                  metadata$field_label[which(metadata$field_name == "f34_1_enddate_syst")]),
                    value2 = ifelse(i == 1, case_when(c07_systtype___1 == 1 ~ c08_chemoyear,
                                                      c07_systtype___2 == 1 ~ c09_targetyear,
                                                      c07_systtype___3 == 1 ~ c10_immyear),
                                    get(paste0("f34_", i-1, "_enddate_syst"))),
                    status = case_when(i == 1 ~ ifelse(value1 > value2, 'PASS','FAIL'),
                                       i > 1 ~ ifelse(value1 >= value2, 'PASS','FAIL')))
  
  if(nrow(data_tmp) != 0)
    data_rule51 <- rbind.fill(data_rule51, data_tmp)
}

### Rule #52 --------------------------------------------------------------------
# End date systemic treatment posterior to Start date systemic treatment
data_rule52 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("f34_[[:digit:]]_enddate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, f107_complete == 1 & !(is.na(get(paste0("f33_", i,"_startdate_syst")))) & !(is.na(get(metadata_tmp$field_name[i])))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "End date of systemic treatment is not posterior to start date of systemic treatment",
                    form1 = "Cancer Under Study Treatment",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(!is.na(get(paste0("f34_", i, "_enddate_syst_unk"))) & format(get(metadata_tmp$field_name[i]), "%d") == "15", get(metadata_tmp$field_name[i]) - 14, get(metadata_tmp$field_name[i])),
                    form2 = "Cancer Under Study Treatment",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == paste0("f33_", i,"_startdate_syst"))],
                    value2 = ifelse(!is.na(get(paste0("f33_", i,"_startdate_syst_unk"))) & format(get(paste0("f33_", i,"_startdate_syst")), "%d") == "15", get(paste0("f33_", i,"_startdate_syst")) + 15, get(paste0("f33_", i,"_startdate_syst"))),
                    status = ifelse(value1 > value2, 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule52 <- rbind.fill(data_rule52, data_tmp)
}

### Rule #53 --------------------------------------------------------------------
# if chemo or immuno  not neoadiuvant  the number must be maximum , equal, 10, if syst neoadiuvant no check
data_rule53 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("f35_[[:digit:]]_numcycle", colnames(redcap_data)) & !grepl("warn", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, f107_complete == 1 & get(metadata_tmp$field_name[i]) != 999 & get(paste0("f30_", i, "_combofsystrt")) %in% c(1,2) & get(paste0("f32_", i, "_systsetting")) != 1), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Number of cycles/administrations is greater than 10",
                    form1 = "Cancer Under Study Treatment",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = get(metadata_tmp$field_name[i]),
                    status = ifelse(value1 <= 10 | value1 == 999, 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule53 <- rbind.fill(data_rule53, data_tmp)
}

### Rule #54 --------------------------------------------------------------------
# If f46_*_reg2 is present, it has to be different from f36_*_regimen
data_rule54 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("f46_[[:digit:]]_reg2", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, f107_complete == 1 & !(is.na(get(metadata_tmp$field_name[i]))) & !(is.na(get(paste0("f36_", i, "_regimen"))))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Regimen changed is not different from previous regimen",
                    form1 = "Cancer Under Study Treatment",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = get(metadata_tmp$field_name[i]),
                    form2 = "Cancer Under Study Treatment",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == paste0("f36_", i, "_regimen"))],
                    value2 = get(paste0("f36_", i, "_regimen")),
                    status = ifelse(value1 != value2, 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule54 <- rbind.fill(data_rule54, data_tmp)
}

### Rule #55 --------------------------------------------------------------------
# Start date regimen changed (f48_1_reg2stdate_syst) posterior to end date systemic treatment (f34_1_enddate_syst)
data_rule55 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("f48_[[:digit:]]_reg2stdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, f107_complete == 1 & !(is.na(get(metadata_tmp$field_name[i]))) & !(is.na(get(paste0("f34_", i, "_enddate_syst"))))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Start date of regimen changed is not posterior to end date of systemic treatment",
                    form1 = "Cancer Under Study Treatment",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(!is.na(paste0("f48_", i, "_reg2stdate_syst_unk")) & format(get(metadata_tmp$field_name[i]), "%d") == "15", get(metadata_tmp$field_name[i]) - 14, get(metadata_tmp$field_name[i])),
                    form2 = "Cancer Under Study Treatment",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == paste0("f34_", i, "_enddate_syst"))],
                    value2 = ifelse(!is.na(get(paste0("f34_", i, "_enddate_syst_unk"))) & format(get(paste0("f34_", i, "_enddate_syst")), "%d") == "15", get(paste0("f34_", i, "_enddate_syst")) + 15, get(paste0("f34_", i, "_enddate_syst"))),
                    status = ifelse(value1 > value2, 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule55 <- rbind.fill(data_rule55, data_tmp)
}

### Rule #56 --------------------------------------------------------------------
# End date regimen changed (f49_1_reg2endate_syst) posterior to Start date regimen changed (f48_1_reg2stdate_syst)
data_rule56 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("f49_[[:digit:]]_reg2endate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, f107_complete == 1 & !(is.na(get(metadata_tmp$field_name[i]))) & !(is.na(get(paste0("f48_", i, "_reg2stdate_syst"))))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "End date of regimen changed is not posterior to start date of regimen changed",
                    form1 = "Cancer Under Study Treatment",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(!is.na(paste0("f49_", i, "_reg2endate_syst_unk")) & format(get(metadata_tmp$field_name[i]), "%d") == "15", get(metadata_tmp$field_name[i]) - 14, get(metadata_tmp$field_name[i])),
                    form2 = "Cancer Under Study Treatment",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == paste0("f48_", i, "_reg2stdate_syst"))],
                    value2 = ifelse(!is.na(get(paste0("f48_", i, "_reg2stdate_syst_unk"))) & format(get(paste0("f48_", i, "_reg2stdate_syst")), "%d") == "15", get(paste0("f48_", i, "_reg2stdate_syst")) + 15, get(paste0("f48_", i, "_reg2stdate_syst"))),
                    status = ifelse(value1 > value2, 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule56 <- rbind.fill(data_rule56, data_tmp)
}

### Rule #57 --------------------------------------------------------------------
# if f54_radiointent is Curative (2), f63_totdose and f66_tothd > 40
data_rule57 <- ddply(subset(redcap_data, f107_complete == 1 & f54_radiointent == 2), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Radiotherapy intent is curative but total dose is lower or equal to 40",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f63_totdose")],
                     value1 = f63_totdose,
                     form2 = "Cancer Under Study Treatment",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "f66_tothd")],
                     value2 = f66_tothd,
                     status = ifelse(value1 > 40 & value2 > 40, 'PASS','FAIL'))

### Rule #58 --------------------------------------------------------------------
# if f55_radiosett Preoperative; Preoperative concomitant to systemic treatment;Postoperative;Postoperative concomitant to systemic treatment the surgery (f01_surgery) should be yes

data_rule58 <- ddply(subset(redcap_data, f107_complete == 1 & f55_radiosett %in% c(1,2,3,4)), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Radiotherapy settings are preoperative or postoperative, but surgery is not performed",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f55_radiosett")],
                     value1 = f55_radiosett,
                     form2 = "Cancer Under Study Treatment",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "f01_surgery")],
                     value2 = f01_surgery,
                     status = ifelse(value2 %in% c(1,2), 'PASS','FAIL'))

### Rule #59 --------------------------------------------------------------------
# if f55_radiosett Preoperative concomitant to systemic treatment;Postoperative concomitant to systemic treatment the systemic treatment (f29_1_systrt) should be yes

data_rule59 <- ddply(subset(redcap_data, f107_complete == 1 & f55_radiosett %in% c(2,4)), c("a01_id"), plyr::summarize,
                     category = "ERROR",
                     description = "Radiotherapy settings are preoperative or postoperative concomitant to systemic treatment, but systemic treatment is not performed",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f55_radiosett")],
                     value1 = f55_radiosett,
                     status = ifelse(any(mget(grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE), inherits = TRUE) %in% c(1,2)) , 'PASS','FAIL'))

### Rule #60 --------------------------------------------------------------------
# if f55_radiosett Definitive concomitant to systemic treatment at least one concomitant

data_rule60 <- ddply(subset(redcap_data, f107_complete == 1 & f55_radiosett == 6), c("a01_id"), plyr::summarize,
                     category = "ERROR",
                     description = "Radiotherapy settings are definitive concomitant to systemic treatment, but systemic treatment is not performed",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f55_radiosett")],
                     value1 = f55_radiosett,
                     status = ifelse(any(mget(grep("f32_[[:digit:]]_systsetting", colnames(redcap_data), value = TRUE), inherits = TRUE) %in% c(2)) , 'PASS','FAIL'))

### Rule #61 --------------------------------------------------------------------
# Start date radiotherapy (f80_radiostartdate) within 9 months from date of diagnosis (d01_diagdate)

data_rule61 <- ddply(subset(redcap_data, f107_complete == 1 & d34_complete == 1 & !(is.na(f80_radiostartdate)) & !(is.na(d01_diagdate))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Start date of radiotherapy is not within 9 months from date of diagnosis",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f80_radiostartdate")],
                     value1 = ifelse(!is.na(f80_radiostartdate_unk) & format(f80_radiostartdate, "%d") == "15", f80_radiostartdate + 15, f80_radiostartdate),
                     form2 = "Cancer under study",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "d01_diagdate")],
                     value2 = ifelse(!is.na(d01_diagdate_unk) & format(d01_diagdate, "%d") == "15", d01_diagdate - 14, d01_diagdate),
                     status = ifelse(getDiffMonths(as.Date(value2, origin = "1970-01-01"), as.Date(value1, origin = "1970-01-01")) >= 0 &
                                       getDiffMonths(as.Date(value2, origin = "1970-01-01"), as.Date(value1, origin = "1970-01-01")) <= 9, 'PASS','FAIL'))

### Rule #62 --------------------------------------------------------------------
# if preoperative or preoperative concomitant to systemic trt date of end radio prior to date of surgery

data_rule62 <- ddply(subset(redcap_data, f107_complete == 1 & f55_radiosett %in% c(1,2) & !(is.na(f02_datesurg)) & !(is.na(f81_radioendate))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "PREOPERATIVE RADIOTHERAPY: end date is not prior to surgery date",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f81_radioendate")],
                     value1 = ifelse(!is.na(f81_radioendate_unk) & format(f81_radioendate, "%d") == "15", f81_radioendate + 15, f81_radioendate),
                     form2 = "Cancer Under Study Treatment",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "f02_datesurg")],
                     value2 = ifelse(!is.na(f02_datesurg_unk) & format(f02_datesurg, "%d") == "15", f02_datesurg - 14, f02_datesurg),
                     status = ifelse(value1 < value2, 'PASS','FAIL'))

### Rule #63 --------------------------------------------------------------------
# if postoperative or postoperative concomitant to systemic trt date of start radio posterior to date of surgery

data_rule63 <- ddply(subset(redcap_data, f107_complete == 1 & f55_radiosett %in% c(3,4) & !(is.na(f02_datesurg)) & !(is.na(f80_radiostartdate))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "POSTOPERATIVE RADIOTHERAPY: start date is not posterior to surgery date",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f80_radiostartdate")],
                     value1 = ifelse(!is.na(f80_radiostartdate_unk) & format(f80_radiostartdate, "%d") == "15", f80_radiostartdate - 14, f80_radiostartdate),
                     form2 = "Cancer Under Study Treatment",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "f02_datesurg")],
                     value2 = ifelse(!is.na(f02_datesurg_unk) & format(f02_datesurg, "%d") == "15", f02_datesurg + 15, f02_datesurg),
                     status = ifelse(value1 > value2, 'PASS','FAIL'))

### Rule #64 --------------------------------------------------------------------
# End date radiotherapy (f81_radioendate) posterior to Start date radiotherapy (f80_radiostartdate)

data_rule64 <- ddply(subset(redcap_data, f107_complete == 1 & !(is.na(f80_radiostartdate)) & !(is.na(f81_radioendate))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "End date of radiotherapy is not posterior to start date of radiotherapy",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f81_radioendate")],
                     value1 = ifelse(!is.na(f81_radioendate_unk) & format(f81_radioendate, "%d") == "15", f81_radioendate - 14, f81_radioendate),
                     form2 = "Cancer Under Study Treatment",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "f80_radiostartdate")],
                     value2 = ifelse(!is.na(f80_radiostartdate_unk) & format(f80_radiostartdate, "%d") == "15", f80_radiostartdate + 15, f80_radiostartdate),
                     status = ifelse(value1 > value2, 'PASS','FAIL'))


### Rule #65 --------------------------------------------------------------------
# f85_radiosite___5 = 1 only if (e33_cm = 2  or e53_pm = 2) or (e34_cstage or e54_pstage = 7) or (Nasopahrynx [d11_siterare, d11_sitecomrar] and e34_cstage or e54_pstage = 7) or (Oropharynx [d11_siterare, d11_sitecomrar] con d27_p16 = 1  and (e34_cstage or e54_pstage = 5)

data_rule65 <- ddply(subset(redcap_data, f107_complete == 1 & d34_complete == 1 & e57_complete == 1 & f85_radiosite___5 == 1), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Treatment site (Distant Metastasis) is not consistent with cM, pM, clinical and pathological staging and site",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f85_radiosite")],
                     value1 = f85_radiosite___5,
                     status = ifelse(any(c(e33_cm, e53_pm) == 2) | any(c(e34_cstage, e54_pstage) == 7) | 
                                       (any(c(d11_siterare, d11_sitecomrar) == 2) & any(c(e34_cstage, e54_pstage) == 7)) | 
                                       (d11_sitecomrar == 8 & d27_p16 == 1 & any(c(e34_cstage, e54_pstage) == 5)), 
                                     "PASS", "FAIL"))

### Rule #66 --------------------------------------------------------------------
# f85_radiosite___5 = 1 at least one f86_radioMsite must be flagged

data_rule66 <- ddply(subset(redcap_data, f107_complete == 1 & f85_radiosite___5 == 1), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Radiotherapy metastatic site is not specified",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f85_radiosite")],
                     value1 = f85_radiosite___5,
                     status = ifelse(any(mget(grep("f86_radiomsite", colnames(redcap_data), value = TRUE), inherits = TRUE) == 1), 
                                     "PASS", "FAIL"))

### Rule #67 --------------------------------------------------------------------
# Start date of other treatment (f100_othertrt_date) within 9 months from date of diagnosis (d01_diagdate)

data_rule67 <- ddply(subset(redcap_data, f107_complete == 1 & d34_complete == 1 & !(is.na(f100_othertrt_date)) & !(is.na(d01_diagdate))), c("a01_id"), summarize,
                     category = "ERROR",
                     description = "Start date of other treatment is not within 9 months from date of diagnosis",
                     form1 = "Cancer Under Study Treatment",
                     event1 = "NON REPEATABLE",
                     instance1 = redcap_repeat_instance,
                     var1 = metadata$field_label[which(metadata$field_name == "f100_othertrt_date")],
                     value1 = f100_othertrt_date,
                     form2 = "Cancer under study",
                     event2 = "NON REPEATABLE",
                     instance2 = redcap_repeat_instance,
                     var2 = metadata$field_label[which(metadata$field_name == "d01_diagdate")],
                     value2 = ifelse(!is.na(d01_diagdate_unk) & format(d01_diagdate, "%d") == "15", d01_diagdate - 14, d01_diagdate),
                     status = ifelse(getDiffMonths(as.Date(value2, origin = "1970-01-01"), as.Date(value1, origin = "1970-01-01")) >= 0 &
                                       getDiffMonths(as.Date(value2, origin = "1970-01-01"), as.Date(value1, origin = "1970-01-01")) <= 9, 'PASS','FAIL'))



### Rule #68 -------------------------------------------------------------------
# g*_03_date after date of diagnosis of the previous instrument (if 1 use cancer under study treatment, if >= 2 use last progression/recurrence)
data_rule68 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_03_date", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !(is.na(get(metadata_tmp$field_name[i])))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Progression/Recurrence/Persistent Disease date is not posterior to previous diagnosis date",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = get(metadata_tmp$field_name[i]),
                    form2 = ifelse(i == 1, "Cancer Under Study", "Progression/recurrence/persistent Disease"),
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = ifelse(i == 1, metadata$field_label[which(metadata$field_name == "d01_diagdate")],
                                  var1),
                    value2 = ifelse(i == 1, d01_diagdate, get(paste0("g", i-1, "_03_date"))),
                    status = ifelse(value1 > value2, 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule68 <- rbind.fill(data_rule68, data_tmp)
}

### Rule #69 -------------------------------------------------------------------
# if g*_06_meta is yes (1), at least one of g*_07_ must be flagged (1)
data_rule69 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_06_meta", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & get(metadata_tmp$field_name[i]) == 1), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "M site is not specified",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = get(metadata_tmp$field_name[i]),
                    status = ifelse(any(mget(grep(paste0("g", i, "_07"), colnames(redcap_data), value = TRUE), inherits = TRUE) == 1), 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule69 <- rbind.fill(data_rule69, data_tmp)
}

### Rule #70 -------------------------------------------------------------------
# Date of surgery posterior or equal to date of progression/recurrence/persistent disease
data_rule70 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_09_datesurg", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !is.na(get(metadata_tmp$field_name[i])) & !is.na(get(paste0("g", i, "_03_date")))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Date of surgery is not posterior to date of progression/recurrence/persistent disease",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(!is.na(get(paste0("g", i, "_09_datesurg_unk"))) & format(get(metadata_tmp$field_name[i]), "%d") == "15", get(metadata_tmp$field_name[i]) - 14, get(metadata_tmp$field_name[i])),
                    form2 = "Progression/recurrence/persistent Disease",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "g1_03_date")],
                    value2 = get(paste0("g", i, "_03_date")),
                    status = ifelse(value1 >= value2, 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule70 <- rbind.fill(data_rule70, data_tmp)
}

### Rule #71 -------------------------------------------------------------------
# Date of neck surgery posterior or equal to date of progression/recurrence/persistent disease
data_rule71 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_16_datenecksurg", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !is.na(get(metadata_tmp$field_name[i])) & !is.na(get(paste0("g", i, "_03_date")))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Date of neck surgery is not posterior to date of progression/recurrence/persistent disease",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(!is.na(get(paste0("g", i, "_16_datenecksurg_unk"))) & format(get(metadata_tmp$field_name[i]), "%d") == "15", get(metadata_tmp$field_name[i]) - 14, get(metadata_tmp$field_name[i])),
                    form2 = "Progression/recurrence/persistent Disease",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "g1_03_date")],
                    value2 = get(paste0("g", i, "_03_date")),
                    status = ifelse(value1 >= value2, 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule71 <- rbind.fill(data_rule71, data_tmp)
}

### Rule #72 -------------------------------------------------------------------
# Number of positive lymph nodes (right side) smaller than removed
data_rule72 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_21_numposlympr", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !is.na(get(metadata_tmp$field_name[i]))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Number of positive lymph nodes (right side) is not smaller than number of removed",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = get(metadata_tmp$field_name[i]),
                    form2 = "Progression/recurrence/persistent Disease",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "g1_20_numlympr")],
                    value2 = get(paste0("g", i, "_20_numlympr")),
                    status = ifelse(value1 < value2, 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule72 <- rbind.fill(data_rule72, data_tmp)
}

### Rule #73 -------------------------------------------------------------------
# Number of positive lymph nodes (left side) smaller than removed
data_rule73 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_23_numposlympl", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !is.na(get(metadata_tmp$field_name[i]))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Number of positive lymph nodes (left side) is not smaller than number of removed",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = get(metadata_tmp$field_name[i]),
                    form2 = "Progression/recurrence/persistent Disease",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "g1_22_numlympl")],
                    value2 = get(paste0("g", i, "_22_numlympl")),
                    status = ifelse(value1 < value2, 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule73 <- rbind.fill(data_rule73, data_tmp)
}

### Rule #74 -------------------------------------------------------------------
# if g*_30_surgm is yes, g*_06_meta must be yes
data_rule74 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_30_surgm", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & get(metadata_tmp$field_name[i]) == 1), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Surgery on M is yes but Metastatic is no at progression/recurrence/persistent disease",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = get(metadata_tmp$field_name[i]),
                    form2 = "Progression/recurrence/persistent Disease",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "g1_06_meta")],
                    value2 = get(paste0("g", i, "_06_meta")),
                    status = ifelse(value2 == 1, 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule74 <- rbind.fill(data_rule74, data_tmp)
}

### Rule #75 -------------------------------------------------------------------
# if if g*_30_surgm is yes (1), at least one of g*_31 must be flagged (1)
data_rule75 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_30_surgm", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & get(metadata_tmp$field_name[i]) == 1), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Site of surgery on M at progression/recurrence/persistent disease is not specified",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = get(metadata_tmp$field_name[i]),
                    status = ifelse(!all(mget(grep(paste0("g", i, "_31_sitesurgm"), colnames(redcap_data), value = TRUE), inherits = TRUE) == 0), 'PASS', 'FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule75 <- rbind.fill(data_rule75, data_tmp)
}

### Rule #76 -------------------------------------------------------------------
# Date of surgery on M posterior or equal to date of progression/recurrence/persistent disease
data_rule76 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_32_datesurgm", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !is.na(get(metadata_tmp$field_name[i]))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Date of surgery on M is not posterior to date of progression/recurrence/persistent disease",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(!is.na(get(paste0("g", i, "_32_datesurgm_unk"))) & format(get(metadata_tmp$field_name[i]), "%d") == "15", get(metadata_tmp$field_name[i]) - 14, get(metadata_tmp$field_name[i])),
                    form2 = "Progression/recurrence/persistent Disease",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "g1_03_date")],
                    value2 = get(paste0("g", i, "_03_date")),
                    status = ifelse(value1 >= value2, 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule76 <- rbind.fill(data_rule76, data_tmp)
}

### Rule #77 -------------------------------------------------------------------
# if concomitant and radiotherapy is not concomitant the next systemic treatment should be compiled and concomitant
data_rule77 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_38_1_systsetting", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  var_tmp2 <- colnames(redcap_data)[grepl(paste0("g", i,"_38_[[:digit:]]_systsetting"), colnames(redcap_data))]
  metadata_tmp2 <- subset(metadata, field_name %in% var_tmp2)
  
  for (j in 1:(nrow(metadata_tmp2) - 1)) {
    data_tmp <- ddply(subset(redcap_data, f107_complete == 1 & get(paste0("g", i, "_38_", j,"_systsetting")) == 2 & !(get(paste0("g", i, "_67_radiosett")) %in% c(2,4,6))), c("a01_id"), summarize,
                      category = "ERROR",
                      description = "Concomitant systemic treatment is not defined",
                      form1 = "Progression/recurrence/persistent Disease",
                      event1 = "NON REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata_tmp2$field_label[j],
                      value1 = get(metadata_tmp2$field_name[j]),
                      status = ifelse(any(mget(var_tmp2[-c(1:j)], inherits = TRUE) == 2) & !all(is.na(mget(var_tmp2[-c(1:j)], inherits = TRUE))), 'PASS', 'FAIL'))
    
    if(nrow(data_tmp) != 0)
      data_rule77 <- rbind.fill(data_rule77, data_tmp)
  }
}

### Rule #78 -------------------------------------------------------------------
data_rule78 <- data.frame()

var_tmp <- grep("g[[:digit:]]+_66_radiointent", colnames(redcap_data), value = TRUE)
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & get(metadata_tmp$field_name[i]) == 2), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Radiotherapy intent in progression/recurrence/persistent disease is curative but total dose and/or total high dose is lower or equal to 40",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata$field_label[which(metadata$field_name == "g1_75_totd")],
                    value1 = get(paste0("g", i, "_75_totd")),
                    form2 = "Progression/recurrence/persistent Disease",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "g1_78_tothd")],
                    value2 = get(paste0("g", i, "_78_tothd")),
                    status = ifelse(value1 > 40 & value2 > 40, 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule78 <- rbind.fill(data_rule78, data_tmp)
}

### Rule #79 -------------------------------------------------------------------
# if neoadiuvant, date must be prior to surgery or start radiotherapy of the same instrument
data_rule79 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_39_1_startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  var_tmp2 <- colnames(redcap_data)[grepl(paste0("g", i,"_39_[[:digit:]]_startdate_syst"), colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
  metadata_tmp2 <- subset(metadata, field_name %in% var_tmp2)
  
  for (j in 1:nrow(metadata_tmp2)) {
    data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & get(paste0("g", i, "_38_", j,"_systsetting")) == 1 & !(is.na(get(metadata_tmp2$field_name[j]))) & (!is.na(get(paste0("g", i, "_09_datesurg"))) | !is.na(get(paste0("g", i, "_92_radiostartdate"))))), c("a01_id"), summarize,
                      category = "ERROR",
                      description = "Neo-adjuvant systemic treatment at progression/recurrence/persistent disease: start date is not prior to surgery or radiotherapy",
                      form1 = "Progression/recurrence/persistent Disease",
                      event1 = "NON REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata_tmp2$field_label[j],
                      value1 = ifelse(!is.na(get(paste0("g", i, "_39_", j,"_startdate_syst_unk"))) & format(get(metadata_tmp2$field_name[j]), "%d") == "15", get(metadata_tmp2$field_name[j]) + 15, get(metadata_tmp2$field_name[j])),
                      form2 = "Progression/recurrence/persistent Disease",
                      event2 = "NON REPEATABLE",
                      instance2 = redcap_repeat_instance,
                      var2 = ifelse(is.na(get(paste0("g", i, "_09_datesurg"))), metadata$field_label[which(metadata$field_name == "g1_92_radiostartdate")], metadata$field_label[which(metadata$field_name == "g1_09_datesurg")]),
                      value2 = ifelse(is.na(get(paste0("g", i, "_09_datesurg"))), get(paste0("g", i, "_92_radiostartdate")), get(paste0("g", i, "_09_datesurg"))),
                      status = ifelse(value1 < value2,'PASS','FAIL'))
    
    if(nrow(data_tmp) != 0)
      data_rule79 <- rbind.fill(data_rule79, data_tmp)
  }
}

### Rule #80 -------------------------------------------------------------------
# if adiuvant, date have to be posterior to surgery or end radiotherapy of the same instrument
data_rule80 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_39_1_startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  var_tmp2 <- colnames(redcap_data)[grepl(paste0("g", i,"_39_[[:digit:]]_startdate_syst"), colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
  metadata_tmp2 <- subset(metadata, field_name %in% var_tmp2)
  
  for (j in 1:nrow(metadata_tmp2)) {
    data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & get(paste0("g", i, "_38_", j,"_systsetting")) == 3 & !(is.na(get(metadata_tmp2$field_name[j]))) & (!is.na(get(paste0("g", i, "_09_datesurg"))) | !is.na(get(paste0("g", i, "_93_radioendate"))))), c("a01_id"), summarize,
                      category = "ERROR",
                      description = "Adjuvant systemic treatment at progression/recurrence/persistent disease: start date is not posterior to surgery or radiotherapy",
                      form1 = "Progression/recurrence/persistent Disease",
                      event1 = "NON REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata_tmp2$field_label[j],
                      value1 = ifelse(!is.na(get(paste0("g", i, "_39_", j,"_startdate_syst_unk"))) & format(get(metadata_tmp2$field_name[j]), "%d") == "15", get(metadata_tmp2$field_name[j]) + 15, get(metadata_tmp2$field_name[j])),
                      form2 = "Progression/recurrence/persistent Disease",
                      event2 = "NON REPEATABLE",
                      instance2 = redcap_repeat_instance,
                      var2 = ifelse(is.na(get(paste0("g", i, "_09_datesurg"))), metadata$field_label[which(metadata$field_name == "g1_93_radioendate")], metadata$field_label[which(metadata$field_name == "g1_09_datesurg")]),
                      value2 = ifelse(is.na(get(paste0("g", i, "_09_datesurg"))), get(paste0("g", i, "_93_radioendate")), get(paste0("g", i, "_09_datesurg"))),
                      status = ifelse(value1 > value2,'PASS','FAIL'))
    
    if(nrow(data_tmp) != 0)
      data_rule80 <- rbind.fill(data_rule80, data_tmp)
  }
}

### Rule #81 --------------------------------------------------------------------
# if concomitant, date should be within the start and end date of radiotherapy (chemo must start 7 gg prior start radio or end 7 gg post end radio)
data_rule81 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_39_1_startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  var_tmp2 <- colnames(redcap_data)[grepl(paste0("g", i,"_39_[[:digit:]]_startdate_syst"), colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
  metadata_tmp2 <- subset(metadata, field_name %in% var_tmp2)
  
  for (j in 1:nrow(metadata_tmp2)) {
    data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & get(paste0("g", i, "_38_", j,"_systsetting")) == 2 & !(is.na(get(metadata_tmp2$field_name[j]))) & get(paste0("g", i, "_67_radiosett")) %in% c(2,4,6) & (!is.na(get(paste0("g", i, "_92_radiostartdate"))) | !is.na(get(paste0("g", i, "_93_radioendate"))))), c("a01_id"), summarize,
                      category = "ERROR",
                      description = "Concomitant systemic treatment at progression/recurrence/persistent disease: start date is not within radiotherapy start and end date",
                      form1 = "Progression/recurrence/persistent Disease",
                      event1 = "NON REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata_tmp2$field_label[j],
                      value1 = ifelse(!is.na(get(paste0("g", i, "_39_", j,"_startdate_syst_unk"))) & format(get(metadata_tmp2$field_name[j]), "%d") == "15", get(metadata_tmp2$field_name[j]) + 15, get(metadata_tmp2$field_name[j])),
                      form2 = "Progression/recurrence/persistent Disease",
                      event2 = "NON REPEATABLE",
                      instance2 = redcap_repeat_instance,
                      var2 = metadata$field_label[which(metadata$field_name == "g1_92_radiostartdate")],
                      value2 = ifelse(!is.na(get(paste0("g", i, "_92_radiostartdate_unk"))) & format(get(paste0("g", i, "_92_radiostartdate")), "%d") == "15", get(paste0("g", i, "_92_radiostartdate")) + 15, get(paste0("g", i, "_92_radiostartdate"))),
                      form3 = "Progression/recurrence/persistent Disease",
                      event3 = "NON REPEATABLE",
                      instance3 = redcap_repeat_instance,
                      var3 = metadata$field_label[which(metadata$field_name == "g1_93_radioendate")],
                      value3 = ifelse(!is.na(get(paste0("g", i, "_93_radioendate_unk"))) & format(get(paste0("g", i, "_93_radioendate")), "%d") == "15", get(paste0("g", i, "_93_radioendate")) - 14, get(paste0("g", i, "_93_radioendate"))),
                      status = ifelse(value1 >= ifelse(get(paste0("g", i,"_36_", j,"_combofsystrt")) == 1, value2 - 7, value2) & value1 <= ifelse(get(paste0("g", i,"_36_", j,"_combofsystrt")) == 1, value3 + 7, value3),
                                      'PASS','FAIL'))
    
    if(nrow(data_tmp) != 0)
      data_rule81 <- rbind.fill(data_rule81, data_tmp)
  }
}

### Rule #82 --------------------------------------------------------------------
# if concomitant to another syst treatment dates must be equals. 
data_rule82 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_39_1_startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  var_tmp2 <- colnames(redcap_data)[grepl(paste0("g", i,"_39_[[:digit:]]_startdate_syst"), colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
  metadata_tmp2 <- subset(metadata, field_name %in% var_tmp2)
  
  for (j in 1:(nrow(metadata_tmp2) - 1)) {
    data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !is.na(get(metadata_tmp2$field_name[j])) & get(paste0("g", i, "_38_" , j, "_systsetting")) == 2 & !(get(paste0("g", i, "_67_radiosett")) %in% c(2,4,6))), c("a01_id"), summarize,
                      category = "ERROR",
                      description = "Start dates of concomitant systemic treatments at progression/recurrence/persistent disease are not the same",
                      form1 = "Progression/recurrence/persistent Disease",
                      event1 = "NON REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata_tmp2$field_label[j],
                      value1 = get(metadata_tmp2$field_name[j]),
                      form2 = "Progression/recurrence/persistent Disease",
                      event2 = "NON REPEATABLE",
                      instance2 = redcap_repeat_instance,
                      var2 = metadata_tmp2$field_label[j],
                      value2 = ifelse(j == 1, ifelse(get(paste0("g", i, "_38_" , j+1, "_systsetting")) == 2, get(metadata_tmp2$field_name[j+1]), get(metadata_tmp2$field_name[j+2])),
                                      get(metadata_tmp2$field_name[j+1])),
                      status = ifelse(value1 == value2, 'PASS', 'FAIL'))
    
    if(nrow(data_tmp) != 0)
      data_rule82 <- rbind.fill(data_rule82, data_tmp)
  }
}

### Rule #83 -------------------------------------------------------------------
# Date posterior to date of progression/recurrence or persistent disease
data_rule83 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_39_1_startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  var_tmp2 <- colnames(redcap_data)[grepl(paste0("g", i,"_39_[[:digit:]]_startdate_syst"), colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
  metadata_tmp2 <- subset(metadata, field_name %in% var_tmp2)
  
  for (j in 1:nrow(metadata_tmp2)) {
    data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !(is.na(get(metadata_tmp2$field_name[j])))), c("a01_id"), summarize,
                      category = "ERROR",
                      description = "Start date of systemic treatment is not posterior to date of progression/recurrence/persistent disease",
                      form1 = "Progression/recurrence/persistent Disease",
                      event1 = "NON REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata_tmp2$field_label[j],
                      value1 = ifelse(!is.na(get(paste0("g", i, "_39_", j,"_startdate_syst_unk"))) & format(get(metadata_tmp2$field_name[j]), "%d") == "15", get(metadata_tmp2$field_name[j]) - 14, get(metadata_tmp2$field_name[j])),
                      form2 = "Progression/recurrence/persistent Disease",
                      event2 = "NON REPEATABLE",
                      instance2 = redcap_repeat_instance,
                      var2 = metadata$field_label[which(metadata$field_name == "g1_03_date")],
                      value2 = get(paste0("g", i, "_03_date")),
                      status = ifelse(value1 > value2,'PASS','FAIL'))
    
    if(nrow(data_tmp) != 0)
      data_rule83 <- rbind.fill(data_rule83, data_tmp)
  }
}

### Rule #84 -------------------------------------------------------------------
# End date systemic treatment posterior to start date
data_rule84 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_40_1_enddate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  var_tmp2 <- colnames(redcap_data)[grepl(paste0("g", i,"_40_[[:digit:]]_enddate_syst"), colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
  metadata_tmp2 <- subset(metadata, field_name %in% var_tmp2)
  
  for (j in 1:nrow(metadata_tmp2)) {
    data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !(is.na(get(metadata_tmp2$field_name[j])))), c("a01_id"), summarize,
                      category = "ERROR",
                      description = "End date of systemic treatment is not posterior to start date",
                      form1 = "Progression/recurrence/persistent Disease",
                      event1 = "NON REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata_tmp2$field_label[j],
                      value1 = ifelse(!is.na(get(paste0("g", i, "_40_", j,"_enddate_syst_unk"))) & format(get(metadata_tmp2$field_name[j]), "%d") == "15", get(metadata_tmp2$field_name[j]) - 14, get(metadata_tmp2$field_name[j])),
                      form2 = "Progression/recurrence/persistent Disease",
                      event2 = "NON REPEATABLE",
                      instance2 = redcap_repeat_instance,
                      var2 = metadata$field_label[which(metadata$field_name == "g1_39_1_startdate_syst")],
                      value2 = ifelse(!is.na(get(paste0("g", i, "_39_", j,"_startdate_syst_unk"))) & format(get(paste0("g", i, "_39_", j,"_startdate_syst")), "%d") == "15", get(paste0("g", i, "_39_", j,"_startdate_syst")) + 15, get(paste0("g", i, "_39_", j,"_startdate_syst"))),
                      status = ifelse(value1 > value2,'PASS','FAIL'))
    
    if(nrow(data_tmp) != 0)
      data_rule84 <- rbind.fill(data_rule84, data_tmp)
  }
}

### Rule #85 -------------------------------------------------------------------
# if chemo or immuno  not neoadiuvant  the number must be maximum , equal, 10, if syst neoadiuvant no check
data_rule85 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_41_1_numcycle", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  var_tmp2 <- colnames(redcap_data)[grepl(paste0("g", i,"_41_[[:digit:]]_numcycle"), colnames(redcap_data))]
  metadata_tmp2 <- subset(metadata, field_name %in% var_tmp2)
  
  for (j in 1:nrow(metadata_tmp2)) {
    data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !(is.na(get(metadata_tmp2$field_name[j]))) & get(metadata_tmp2$field_name[j]) != 999 & get(paste0("g", i, "_36_", j,"_combofsystrt")) %in% c(1,2)  & get(paste0("g", i, "_38_", j,"_systsetting")) != 1), c("a01_id"), summarize,
                      category = "ERROR",
                      description = "Number of cycles/administration is greater than 10",
                      form1 = "Progression/recurrence/persistent Disease",
                      event1 = "NON REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata_tmp2$field_label[j],
                      value1 = get(metadata_tmp2$field_name[j]),
                      status = ifelse(value1 <= 10, 'PASS', 'FAIL'))
    
    if(nrow(data_tmp) != 0)
      data_rule85 <- rbind.fill(data_rule85, data_tmp)
  }
}

### Rule #86 -------------------------------------------------------------------
# g*_54 Different from g*_42 if g*_53=yes
data_rule86 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_54_1_reg2", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  var_tmp2 <- colnames(redcap_data)[grepl(paste0("g", i,"_54_[[:digit:]]_reg2"), colnames(redcap_data))]
  metadata_tmp2 <- subset(metadata, field_name %in% var_tmp2)
  
  for (j in 1:nrow(metadata_tmp2)) {
    data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !(is.na(get(metadata_tmp2$field_name[j]))) & get(paste0("g", i, "_53_", j,"_regchange")) == 1 & !is.na(get(paste0("g", i, "_42_", j,"_regimen")))), c("a01_id"), summarize,
                      category = "ERROR",
                      description = "Regimen changed is not different from previous regimen",
                      form1 = "Progression/recurrence/persistent Disease",
                      event1 = "NON REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata_tmp2$field_label[j],
                      value1 = get(metadata_tmp2$field_name[j]),
                      form2 = "Progression/recurrence/persistent Disease",
                      event2 = "NON REPEATABLE",
                      instance2 = redcap_repeat_instance,
                      var2 = metadata$field_label[which(metadata$field_name == "g1_42_1_regimen")],
                      value2 = get(paste0("g", i, "_42_", j,"_regimen")),
                      status = ifelse(value1 != value2, 'PASS', 'FAIL'))
    
    if(nrow(data_tmp) != 0)
      data_rule86 <- rbind.fill(data_rule86, data_tmp)
  }
}

### Rule #87 -------------------------------------------------------------------
# Start date regimen changed posterior to end date systemic treatment
data_rule87 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_58_1_reg2startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  var_tmp2 <- colnames(redcap_data)[grepl(paste0("g", i,"_58_[[:digit:]]_reg2startdate_syst"), colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
  metadata_tmp2 <- subset(metadata, field_name %in% var_tmp2)
  
  for (j in 1:nrow(metadata_tmp2)) {
    data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !(is.na(get(metadata_tmp2$field_name[j]))) & !is.na(get(paste0("g", i, "_40_", j,"_enddate_syst")))), c("a01_id"), summarize,
                      category = "ERROR",
                      description = "Start date of regimen changed is not posterior to end date of systemic treatment",
                      form1 = "Progression/recurrence/persistent Disease",
                      event1 = "NON REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata_tmp2$field_label[j],
                      value1 = ifelse(!is.na(get(paste0("g", i, "_58_", j,"_reg2startdate_syst_unk"))) & format(get(metadata_tmp2$field_name[j]), "%d") == "15", get(metadata_tmp2$field_name[j]) - 14, get(metadata_tmp2$field_name[j])),
                      form2 = "Progression/recurrence/persistent Disease",
                      event2 = "NON REPEATABLE",
                      instance2 = redcap_repeat_instance,
                      var2 = metadata$field_label[which(metadata$field_name == "g1_40_1_enddate_syst")],
                      value2 = ifelse(!is.na(get(paste0("g", i, "_40_", j,"_enddate_syst_unk"))) & format(get(paste0("g", i, "_40_", j,"_enddate_syst")), "%d") == "15", get(paste0("g", i, "_40_", j,"_enddate_syst")) + 15, get(paste0("g", i, "_40_", j,"_enddate_syst"))),
                      status = ifelse(value1 > value2,'PASS','FAIL'))
    
    if(nrow(data_tmp) != 0)
      data_rule87 <- rbind.fill(data_rule87, data_tmp)
  }
}

### Rule #88 -------------------------------------------------------------------
# End date regimen changed posterior to start date
data_rule88 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_59_1_reg2enddate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  var_tmp2 <- colnames(redcap_data)[grepl(paste0("g", i,"_59_[[:digit:]]_reg2enddate_syst"), colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
  metadata_tmp2 <- subset(metadata, field_name %in% var_tmp2)
  
  for (j in 1:nrow(metadata_tmp2)) {
    data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !(is.na(get(metadata_tmp2$field_name[j]))) & !is.na(get(paste0("g", i, "_58_", j,"_reg2startdate_syst")))), c("a01_id"), summarize,
                      category = "ERROR",
                      description = "End date of regimen changed is not posterior to start date",
                      form1 = "Progression/recurrence/persistent Disease",
                      event1 = "NON REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata_tmp2$field_label[j],
                      value1 = ifelse(!is.na(get(paste0("g", i, "_59_", j,"_reg2enddate_syst_unk"))) & format(get(metadata_tmp2$field_name[j]), "%d") == "15", get(metadata_tmp2$field_name[j]) - 14, get(metadata_tmp2$field_name[j])),
                      form2 = "Progression/recurrence/persistent Disease",
                      event2 = "NON REPEATABLE",
                      instance2 = redcap_repeat_instance,
                      var2 = metadata$field_label[which(metadata$field_name == "g1_58_1_reg2startdate_syst")],
                      value2 = ifelse(!is.na(get(paste0("g", i, "_58_", j,"_reg2startdate_syst_unk"))) & format(get(paste0("g", i, "_58_", j,"_reg2startdate_syst")), "%d") == "15", get(paste0("g", i, "_58_", j,"_reg2startdate_syst")) + 15, get(paste0("g", i, "_58_", j,"_reg2startdate_syst"))),
                      status = ifelse(value1 > value2,'PASS','FAIL'))
    
    if(nrow(data_tmp) != 0)
      data_rule88 <- rbind.fill(data_rule88, data_tmp)
  }
}

### Rule #89 -------------------------------------------------------------------
# if reirradioation(1)=yes and radiotherapy in cancer under study treatment = no or unknown c'è un errore. Se andiamo avanti con progressione o recurrence e reirradiation=yes almeno una precedente deve aver radioterapia =yes
data_rule89 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_64_reradiation", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & f107_complete == 1 & get(metadata_tmp$field_name[i]) == 1), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Reirradiation is yes but there is not a previous radiotherapy specified",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = get(metadata_tmp$field_name[i]),
                    status = ifelse(i == 1, ifelse(f53_radio %in% c(1,2), 'PASS', 'FAIL'),
                                    ifelse(any(mget(c("f53_radio", grep("g[[:digit:]]+_63_radio", colnames(redcap_data), value = TRUE)[1:i-1]), inherits = TRUE) %in% c(1,2)), 'PASS', 'FAIL')))
  
  if(nrow(data_tmp) != 0)
    data_rule89 <- rbind.fill(data_rule89, data_tmp)
}

### Rule #90 -------------------------------------------------------------------
# if radiotherapy pre o postoperative surgery should be yes
data_rule90 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_67_radiosett", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & get(metadata_tmp$field_name[i]) %in% c(1:4)), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Radiotherapy is pre/post-operative but surgery is not defined",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = get(metadata_tmp$field_name[i]),
                    status = ifelse(get(paste0("g", i, "_08_surgery")) %in% c(1,2), 'PASS', 'FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule90 <- rbind.fill(data_rule90, data_tmp)
}

### Rule #91 -------------------------------------------------------------------
# if radiotherapy concomitant to sys trt, systemic treatment should be yes
data_rule91 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_67_radiosett", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & get(metadata_tmp$field_name[i]) %in% c(2,4,6)), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Radiotherapy is concomitant to systemic treatment but treatment is not defined",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = get(metadata_tmp$field_name[i]),
                    status = ifelse(any(mget(grep(paste0("g", i, "_38_[[:digit:]]_systsetting"), colnames(redcap_data), value = TRUE), inherits = TRUE) == 2) &
                                      !all(is.na(mget(grep(paste0("g", i, "_38_[[:digit:]]_systsetting"), colnames(redcap_data), value = TRUE), inherits = TRUE))), 'PASS', 'FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule91 <- rbind.fill(data_rule91, data_tmp)
}

### Rule #92 -------------------------------------------------------------------
#  if radiotherapy "preoperative" or "preoperative concomitant to systemic trt", date of end radio prior to date of surgery
data_rule92 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_93_radioendate", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !is.na(get(metadata_tmp$field_name[i])) & get(paste0("g", i, "_67_radiosett")) %in% c(1,2)), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Preoperative radiotherapy end date is not prior to surgery",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(!is.na(get(paste0("g", i, "_93_radioendate_unk"))) & format(get(metadata_tmp$field_name[i]), "%d") == "15", get(metadata_tmp$field_name[i]) + 15, get(metadata_tmp$field_name[i])),
                    form2 = "Progression/recurrence/persistent Disease",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "g1_09_datesurg")],
                    value2 = ifelse(!is.na(get(paste0("g", i, "_09_datesurg_unk"))) & format(get(paste0("g", i, "_09_datesurg")), "%d") == "15", get(paste0("g", i, "_09_datesurg")) - 14, get(paste0("g", i, "_09_datesurg"))),
                    status = ifelse(value1 < value2, 'PASS', 'FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule92 <- rbind.fill(data_rule92, data_tmp)
}

### Rule #93 -------------------------------------------------------------------
#  if radiotherapy "postoperative" or "postoperative concomitant to systemic trt",  date of start radio posterior to date of surgery .
data_rule93 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_92_radiostartdate", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !is.na(get(metadata_tmp$field_name[i])) & get(paste0("g", i, "_67_radiosett")) %in% c(3,4)), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Postoperative radiotherapy start date is not posterior to surgery",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(!is.na(get(paste0("g", i, "_92_radiostartdate_unk"))) & format(get(metadata_tmp$field_name[i]), "%d") == "15", get(metadata_tmp$field_name[i]) - 14, get(metadata_tmp$field_name[i])),
                    form2 = "Progression/recurrence/persistent Disease",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "g1_09_datesurg")],
                    value2 = ifelse(!is.na(get(paste0("g", i, "_09_datesurg_unk"))) & format(get(paste0("g", i, "_09_datesurg")), "%d") == "15", get(paste0("g", i, "_09_datesurg")) + 15, get(paste0("g", i, "_09_datesurg"))),
                    status = ifelse(value1 > value2, 'PASS', 'FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule93 <- rbind.fill(data_rule93, data_tmp)
}

### Rule #94 -------------------------------------------------------------------
#  Date of start radiotherapy posterior to date of progression/recurrence /persistent disease 
data_rule94 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_92_radiostartdate", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !is.na(get(metadata_tmp$field_name[i])) & !is.na(get(paste0("g", i, "_03_date")))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Radiotherapy start date is not posterior to date of progression/recurrence/persistent disease",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(!is.na(get(paste0("g", i, "_92_radiostartdate_unk"))) & format(get(metadata_tmp$field_name[i]), "%d") == "15", get(metadata_tmp$field_name[i]) - 14, get(metadata_tmp$field_name[i])),
                    form2 = "Progression/recurrence/persistent Disease",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "g1_03_date")],
                    value2 = get(paste0("g", i, "_03_date")),
                    status = ifelse(value1 > value2, 'PASS', 'FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule94 <- rbind.fill(data_rule94, data_tmp)
}

### Rule #95 -------------------------------------------------------------------
#  End date of radiotherapy posterior to start date
data_rule95 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_93_radioendate", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !is.na(get(metadata_tmp$field_name[i])) & !is.na(get(paste0("g", i, "_92_radiostartdate")))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Radiotherapy end date is not posterior to start date",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(!is.na(get(paste0("g", i, "_93_radioendate_unk"))) & format(get(metadata_tmp$field_name[i]), "%d") == "15", get(metadata_tmp$field_name[i]) - 14, get(metadata_tmp$field_name[i])),
                    form2 = "Progression/recurrence/persistent Disease",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "g1_92_radiostartdate")],
                    value2 = ifelse(!is.na(get(paste0("g", i, "_92_radiostartdate_unk"))) & format(get(paste0("g", i, "_92_radiostartdate")), "%d") == "15", get(paste0("g", i, "_92_radiostartdate")) + 15, get(paste0("g", i, "_92_radiostartdate"))),
                    status = ifelse(value1 > value2, 'PASS', 'FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule95 <- rbind.fill(data_rule95, data_tmp)
}

### Rule #96 -------------------------------------------------------------------
#  if g*_97_radiosite___5 = yes, g*_06_meta must be yes
data_rule96 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_97_radiosite___5", colnames(redcap_data))]

for (i in 1:length(var_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & get(var_tmp[i]) == 1), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Treatment site is Distant Metastatis but patient is not metastatic",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata$field_label[which(metadata$field_name == "g1_97_radiosite")],
                    value1 = get(var_tmp[i]),
                    form2 = "Progression/recurrence/persistent Disease",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "g1_06_meta")],
                    value2 = get(paste0("g", i, "_06_meta")),
                    status = ifelse(value2 == 1, 'PASS', 'FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule96 <- rbind.fill(data_rule96, data_tmp)
}

### Rule #97 -------------------------------------------------------------------
#  if distant metastasis (g*_97_radiosite___5) yes at least one g*_98_radiomsite flagged
data_rule97 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_97_radiosite___5", colnames(redcap_data))]

for (i in 1:length(var_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & get(var_tmp[i]) == 1), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Treatment site is Distant Metastatis but metastatic treatment site is not specified",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata$field_label[which(metadata$field_name == "g1_97_radiosite")],
                    value1 = get(var_tmp[i]),
                    status = ifelse(any(mget(grep(paste0("g", i, "_98_radiomsite"), colnames(redcap_data), value = TRUE), inherits = TRUE) == 1), 'PASS', 'FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule97 <- rbind.fill(data_rule97, data_tmp)
}

### Rule #98 ------------------------------------------------------------------
# Date of other treatment posterior or equal to date of progression/recurrence/persistent disease
data_rule98 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_112_othertrt_date", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data)) & !grepl("end", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:nrow(metadata_tmp)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & !is.na(get(metadata_tmp$field_name[i])) & !is.na(get(paste0("g", i, "_03_date")))), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Date of other treatment is not posterior to date of progression/recurrence/persistent disease",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = ifelse(!is.na(get(paste0("g", i, "_112_othertrt_date_unk"))) & format(get(metadata_tmp$field_name[i]), "%d") == "15", get(metadata_tmp$field_name[i]) - 14, get(metadata_tmp$field_name[i])),
                    form2 = "Progression/recurrence/persistent Disease",
                    event2 = "NON REPEATABLE",
                    instance2 = redcap_repeat_instance,
                    var2 = metadata$field_label[which(metadata$field_name == "g1_03_date")],
                    value2 = get(paste0("g", i, "_03_date")),
                    status = ifelse(value1 >= value2, 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule98 <- rbind.fill(data_rule98, data_tmp)
}
### Rule #99 ------------------------------------------------------------------
# If treatment response is progression a next progression relapse should be completed
data_rule99 <- data.frame()

var_tmp <- colnames(redcap_data)[grepl("g[[:digit:]]+_115_overalltrt_resp", colnames(redcap_data))]
metadata_tmp <- subset(metadata, field_name %in% var_tmp)

for (i in 1:(nrow(metadata_tmp) - 1)) {
  data_tmp <- ddply(subset(redcap_data, get(paste0("g", i, "_119_complete")) == 1 & get(metadata_tmp$field_name[i]) == 4), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Treatment response is progression but next progression relapse is not defined",
                    form1 = "Progression/recurrence/persistent Disease",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata_tmp$field_label[i],
                    value1 = get(metadata_tmp$field_name[i]),
                    status = ifelse(!is.na(get(paste0("g", i+1, "_01_progrel"))), 'PASS','FAIL'))
  
  if(nrow(data_tmp) != 0)
    data_rule99 <- rbind.fill(data_rule99, data_tmp)
}

### Rule #100 -------------------------------------------------------------------
# Date of last follow-up (h02_datelasfup) > date of diagnosis (d01_diagdate)

data_rule100 <- ddply(subset(redcap_data, h06_complete == 1 & d34_complete == 1 & !(is.na(h02_datelasfup)) & !(is.na(d01_diagdate))), c("a01_id"), summarize,
                      category = "ERROR",
                      description = "Date of last follow-up is not posterior to date of diagnosis",
                      form1 = "Status of patient at fup",
                      event1 = "NON REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata$field_label[which(metadata$field_name == "h02_datelasfup")],
                      value1 = h02_datelasfup,
                      form2 = "Cancer under study",
                      event2 = "NON REPEATABLE",
                      instance2 = redcap_repeat_instance,
                      var2 = metadata$field_label[which(metadata$field_name == "d01_diagdate")],
                      value2 = ifelse(!is.na(d01_diagdate_unk) & format(d01_diagdate, "%d") == "15", d01_diagdate + 15, d01_diagdate),
                      status = ifelse(value1 > value2, 'PASS','FAIL'))

### Rule #101 -------------------------------------------------------------------
# Date of last follow-up (h02_datelasfup) > last date of progression/recurrence/persistent disease

data_rule101 <- ddply(subset(redcap_data, h06_complete == 1 & !(is.na(h02_datelasfup))), c("a01_id"), summarize,
                      category = "ERROR",
                      description = "Date of last follow-up is not posterior to last date of progression/recurrence/persistent disease",
                      form1 = "Status of patient at fup",
                      event1 = "NON REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata$field_label[which(metadata$field_name == "h02_datelasfup")],
                      value1 = h02_datelasfup,
                      form2 = "Progression/recurrence/persistent Disease",
                      event2 = "NON REPEATABLE",
                      instance2 = redcap_repeat_instance,
                      var2 = metadata$field_label[which(metadata$field_name == "g1_03_date")],
                      value2 = tail(mget(grep("g[[:digit:]]+_03_date",colnames(redcap_data), value = TRUE), inherits = TRUE)[!is.na(mget(grep("g[[:digit:]]+_03_date",colnames(redcap_data), value = TRUE), inherits = TRUE))], 1),
                      status = ifelse(value1 > value2, 'PASS','FAIL'))

### Rule #102 --------------------------------------------------------------------
# Date of new cancer (h04_newcancer_date) > date of diagnosis (d01_diagdate)

data_rule102 <- ddply(subset(redcap_data, h06_complete == 1 & d34_complete == 1 & !(is.na(h04_newcancer_date)) & !(is.na(d01_diagdate))), c("a01_id"), summarize,
                      category = "ERROR",
                      description = "Date of new cancer is not posterior to date of diagnosis",
                      form1 = "Status of patient at fup",
                      event1 = "NON REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata$field_label[which(metadata$field_name == "h04_newcancer_date")],
                      value1 = h04_newcancer_date,
                      form2 = "Cancer under study",
                      event2 = "NON REPEATABLE",
                      instance2 = redcap_repeat_instance,
                      var2 = metadata$field_label[which(metadata$field_name == "d01_diagdate")],
                      value2 = ifelse(!is.na(d01_diagdate_unk) & format(d01_diagdate, "%d") == "15", d01_diagdate + 15, d01_diagdate),
                      status = ifelse(value1 > value2, 'PASS','FAIL'))

### Rule #103 --------------------------------------------------------------------
# Date of new cancer (h04_newcancer_date) <= date of last follow-up (h02_datelasfup)

data_rule103 <- ddply(subset(redcap_data, h06_complete == 1 & !(is.na(h04_newcancer_date)) & !(is.na(h02_datelasfup))), c("a01_id"), summarize,
                      category = "ERROR",
                      description = "Date of new cancer is posterior to date of last follow-up",
                      form1 = "Adverse events",
                      event1 = "REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata$field_label[which(metadata$field_name == "h04_newcancer_date")],
                      value1 = h04_newcancer_date,
                      form2 = "Status of patient at fup",
                      event2 = "NON REPEATABLE",
                      instance2 = redcap_repeat_instance,
                      var2 = metadata$field_label[which(metadata$field_name == "h02_datelasfup")],
                      value2 = h02_datelasfup,
                      status = ifelse(value1 <= value2, 'PASS','FAIL'))

### Rule #104 --------------------------------------------------------------------
# if adverse event baseline and chemotherapy, chemotherapy at baseline should be yes, and so on for all the combination

data_rule104 <- ddply(subset(redcap_data, redcap_repeat_instrument == "adverse_events" & i07_complete == 1 & !is.na(i03_adve_when) & !is.na(i04_adve_relto)), c("a01_id", "redcap_repeat_instance"), summarize,
                      category = "ERROR",
                      description = "Adverse event occurrence and related therapy are not consistent with previous data",
                      form1 = "Adverse events",
                      event1 = "REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata$field_label[which(metadata$field_name == "i03_adve_when")],
                      value1 = i03_adve_when,
                      form2 = "Adverse events",
                      event2 = "REPEATABLE",
                      instance2 = redcap_repeat_instance,
                      var2 = metadata$field_label[which(metadata$field_name == "i04_adve_relto")],
                      value2 = i04_adve_relto,
                      status = case_when(value1 == 1 & value2 == 1 ~ ifelse(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), grep("f30_[[:digit:]]_combofsystrt", colnames(redcap_data))] == 1) & 
                                                                              !is.na(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), grep("f30_[[:digit:]]_combofsystrt", colnames(redcap_data))] == 1)),
                                                                            'PASS', 'FAIL'),
                                         value1 == 1 & value2 == 2 ~ ifelse(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), "f53_radio"] %in% c(1,2)) &
                                                                              !is.na(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), "f53_radio"] %in% c(1,2))),
                                                                            'PASS', 'FAIL'),
                                         value1 == 1 & value2 == 3 ~ ifelse(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), grep("f30_[[:digit:]]_combofsystrt", colnames(redcap_data))] == 2) &
                                                                              !is.na(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), grep("f30_[[:digit:]]_combofsystrt", colnames(redcap_data))] == 2)),
                                                                            'PASS', 'FAIL'),
                                         value1 == 1 & value2 == 4 ~ ifelse(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), grep("f30_[[:digit:]]_combofsystrt", colnames(redcap_data))] == 3) &
                                                                              !is.na(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), grep("f30_[[:digit:]]_combofsystrt", colnames(redcap_data))] == 3)),
                                                                            'PASS', 'FAIL'),
                                         value1 != 1 & value2 == 1 ~ ifelse(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), grep(paste0("g", value1 - 1, "_36_[[:digit:]]_combofsystrt"), colnames(redcap_data))] == 1) & 
                                                                              !is.na(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), grep(paste0("g", value1 - 1, "_36_[[:digit:]]_combofsystrt"), colnames(redcap_data))] == 1)),
                                                                            'PASS', 'FAIL'),
                                         value1 != 1 & value2 == 2 ~ ifelse(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), paste0("g", value1 - 1,"_63_radio")] %in% c(1,2)) &
                                                                              !is.na(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), paste0("g", value1 - 1,"_63_radio")] %in% c(1,2))),
                                                                            'PASS', 'FAIL'),
                                         value1 != 1 & value2 == 3 ~ ifelse(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), grep(paste0("g", value1 - 1, "_36_[[:digit:]]_combofsystrt"), colnames(redcap_data))] == 2) & 
                                                                              !is.na(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), grep(paste0("g", value1 - 1, "_36_[[:digit:]]_combofsystrt"), colnames(redcap_data))] == 2)),
                                                                            'PASS', 'FAIL'),
                                         value1 != 1 & value2 == 4 ~ ifelse(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), grep(paste0("g", value1 - 1, "_36_[[:digit:]]_combofsystrt"), colnames(redcap_data))] == 3) & 
                                                                              !is.na(any(redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), grep(paste0("g", value1 - 1, "_36_[[:digit:]]_combofsystrt"), colnames(redcap_data))] == 3)),
                                                                            'PASS', 'FAIL'),
                                         TRUE ~ "")) %>%
  select(-redcap_repeat_instance)


### Rule #105 --------------------------------------------------------------------
# adverse event start date posterior to date of diagnosis and prior to last follow-up

data_rule105 <- ddply(subset(redcap_data, redcap_repeat_instrument == "adverse_events" & i07_complete == 1 & !is.na(i05_adve_startdate)), c("a01_id", "redcap_repeat_instance"), summarize,
                      category = "ERROR",
                      description = "Adverse event start date is not between date of diagnosis and date of last follow-up",
                      form1 = "Adverse events",
                      event1 = "REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata$field_label[which(metadata$field_name == "i05_adve_startdate")],
                      value1 = i05_adve_startdate,
                      form2 = "Cancer under study",
                      event2 = "NON REPEATABLE",
                      instance2 = NA,
                      var2 = metadata$field_label[which(metadata$field_name == "d01_diagdate")],
                      value2 = redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), "d01_diagdate"],
                      form3 = "Status of patient at fup",
                      event3 = "NON REPEATABLE",
                      instance3 = NA,
                      var3 = metadata$field_label[which(metadata$field_name == "h02_datelasfup")],
                      value3 = redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), "h02_datelasfup"],
                      status = ifelse(value2 < value1 & value1 < value3, 'PASS', 'FAIL')) %>% 
  select(-redcap_repeat_instance)

### Rule #106 --------------------------------------------------------------------
# If occurred at baseline, date of adverse event prior to date progression/recurrence/persistent disease, if occurred at progression/relapse/persistent disease number 1, date of adverse event starting must be posterior to date  of progression/recurrence/persistent disease number 1 and prior to date of progression/recurrence/persistent disease number 2 if present (and so on for the other possible 10 forms)

data_rule106 <- ddply(subset(redcap_data, redcap_repeat_instrument == "adverse_events" & i07_complete == 1 & !is.na(i05_adve_startdate)), c("a01_id", "redcap_repeat_instance"), summarize,
                      category = "ERROR",
                      description = "Adverse event start date is not prior to date of subsequent progression/recurrence/persistent disease",
                      form1 = "Adverse events",
                      event1 = "REPEATABLE",
                      instance1 = redcap_repeat_instance,
                      var1 = metadata$field_label[which(metadata$field_name == "i05_adve_startdate")],
                      value1 = i05_adve_startdate,
                      form2 = "Progression/recurrence/persistent Disease",
                      event2 = "NON REPEATABLE",
                      instance2 = NA,
                      var2 = metadata$field_label[which(metadata$field_name == "g1_03_date")],
                      value2 = ifelse(paste0("g", i03_adve_when, "_03_date") %in% colnames(redcap_data),
                                      redcap_data[which(redcap_data$a01_id == a01_id & redcap_data$redcap_event_name == "non_repeatable_arm_1" & is.na(redcap_data$redcap_repeat_instance)), paste0("g", i03_adve_when, "_03_date")],
                                      NA),
                      status = ifelse(value1 < value2, 'PASS', 'FAIL')) %>% 
  select(-redcap_repeat_instance)

# join data and select only failed check ---------------------------------------

data_rules <- ls(all.names = FALSE, pattern = "data_rule") # list of data_rule*
data_rules <- data_rules[order(nchar(data_rules), data_rules)] # sort in sequential order
data_rules <- mget(data_rules) # create a list
data_rules <- Filter(function(x) dim(x)[1] > 0, data_rules) # remove empty dataframes

check_failed <- do.call(rbind.fill, data_rules) %>% select(- status, everything()) %>% 
  subset(status == 'FAIL') %>% select(-c(grep("value", colnames(.)), status))

# write and save individual report ---------------------------------------------
# create workbook
wb <- createWorkbook()

# add worksheet
addWorksheet(wb, "Checks")
addWorksheet(wb, "Legend")

if(!(empty(check_failed))){
  # if not empty build new dataframe
  err_regole <- summary.rules(check_failed, method = "patients")
  
  # set new columns names
  data.table::setnames(err_regole, old = c("category","description", "freq"), new = c("Rule category", "Rule description", "Total"))
  
  # get checks legend
  legend <- getLegend(check_failed)
  columnNames <- colnames(legend) %>% 
    gsub("([[:alpha:]]+)([[:digit:]])", "\\1 \\2", .) %>% 
    gsub("var", "Variable", .) %>%
    gsub("event", "Event", .) %>% 
    gsub("form", "Form", .) %>% 
    gsub("category", "Rule category", .) %>%
    gsub("description", "Rule description", .)
  colnames(legend) <- columnNames
  
  # write data on worksheet
  writeData(wb,sheet = "Checks", err_regole, borders = "all", borderColour = "#999999",
            headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))                             
  setColWidths(wb,sheet = "Checks",cols = 1:ncol(err_regole),widths = "auto")
  
  writeData(wb,sheet = "Legend", legend, borders = "all", borderColour = "#999999",
            headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))                             
  setColWidths(wb,sheet = "Legend",cols = 1:ncol(legend),widths = "auto")
} else {
  # if empty write "No errors found" on the excel file
  writeData(wb,sheet = "Checks", "No errors found") 
}

# write individual report
file_name <- paste0(properties$id_centro,'-',id_analysis,'-QC-individual-', format(Sys.Date(), "%y%m%d"), '.xlsx')

# save to xlsx
saveWorkbook(wb, file_name, overwrite = TRUE)

# Copy in /data
file.copy(from = paste0("/opt/redcap_dq/environment/scripts/", file_name), "/opt/redcap_dq/environment/data")

file.copy(from = paste0("/opt/redcap_dq/environment/data/", file_name), "/var/lib/docker/volumes/vantage6-starter_head_and_neck-user-vol/_data")
file.rename(from = paste0("/var/lib/docker/volumes/vantage6-starter_head_and_neck-user-vol/_data/", file_name),
            to = paste0("/var/lib/docker/volumes/vantage6-starter_head_and_neck-user-vol/_data/", ID_alg,"-QC-individual.xlsx"))

############################################## QC REPORT SUMMARY ##############################################
# Missing and unknown ---------
# create a new dataframe to count % of missing and % of missing+unknown
check_summary <- data.frame("var" = character(0), "var_lab" = character(0), "form" = character(0), "den" = integer(0), "perc_miss" = double(0), "perc_miss_unk" = double(0))

## Select variables with field type != descriptive, calculated or checkbox
metadata_tmp <- subset(metadata,!(field_type %in% c("descriptive","calc", "checkbox"))) %>%
  # tolgo anche a01_id e tutte quelle del tipo *_dayunk
  filter(!(field_name %in% c("a01_id", grep("(date)([[:graph:]]*)(_unk)", metadata$field_name, value = TRUE)))) %>%
  # tolgo altre variabili per cui non devo controllare missing -------- INSERIRE QUI VARIABILI PER CUI NON SI VUOLE CHECK SU MISSING E UNKNOWN
  filter(!(field_name %in% c("e03_ct", "e04_mri", "e05_us", "e06_fdgpet", "e07_pet", "e08_opticdig", "e09_ctpet", "e10_unkim", "e11_otherim",
                             "e13_ct_neck", "e14_mri_neck", "e15_us_neck", "e16_fdgpet_neck", "e17_pet_neck", "e18_ctpet_neck", "e19_unkim_neck", "e20_otherim_neck",
                             "e22_ct_m", "e23_mri_m", "e24_us_m", "e25_fdgpet_m", "e26_pet_m", "e27_ctpet_m", "e28_unkim_m", "e29_otherim_m")))

for (i in 1:nrow(metadata_tmp)){
  var_tmp = metadata_tmp$field_name[i]
  form_complete = ifelse(substring(var_tmp,1,1) == "g",
                         grep(paste0(substring(var_tmp,1,2),"_[[:digit:]]+_complete"), metadata$field_name, value = TRUE),
                         grep(paste0(substring(var_tmp,1,1),"[[:digit:]]+_complete"), metadata$field_name, value = TRUE))
  expr_str = renderLogic(metadata_tmp$branching_logic[i])
  if(!is.na(form_complete)){
    if(is.na(expr_str))
      ps_tmp <- subset(redcap_data, get(form_complete) == 1)
    else
      ps_tmp <- subset(redcap_data, get(form_complete) == 1 & eval(parse(text = expr_str)))
  } else {
    event <- case_when(substring(var_tmp,1,1) == "j" ~ "imaging_available",
                       substring(var_tmp,1,1) == "k" ~ "tumor_specimen_available",
                       substring(var_tmp,1,1) == "l" ~ "gene_test_expression_analysis")

    if(is.na(expr_str))
      ps_tmp <- subset(redcap_data, redcap_repeat_instrument == event)
    else
      ps_tmp <- subset(redcap_data, redcap_repeat_instrument == event & eval(parse(text = expr_str)))
  }

  if(nrow(ps_tmp) != 0){
    check_summary <- getMissUnk(ps_tmp, var_tmp, check_summary)
  }
  
  gc()
}

## Checkbox
metadata_tmp <- subset(metadata, field_type %in% c("checkbox"))

for (i in 1:nrow(metadata_tmp)){
  var_patt = metadata_tmp$field_name[i]
  form_complete = ifelse(substring(var_tmp,1,1) == "g",
                         grep(paste0(substring(var_patt,1,2),"_[[:digit:]]+_complete"), metadata$field_name, value = TRUE),
                         grep(paste0(substring(var_patt,1,1),"[[:digit:]]+_complete"), metadata$field_name, value = TRUE))
  expr_str = renderLogic(metadata_tmp$branching_logic[i])
  if(!is.na(form_complete)){
    if(is.na(expr_str))
      ps_tmp <- subset(redcap_data, get(form_complete) == 1)
    else
      ps_tmp <- subset(redcap_data, get(form_complete) == 1 & eval(parse(text = expr_str)))
  } else {
    event <- case_when(substring(var_patt,1,1) == "j" ~ "imaging_available",
                       substring(var_patt,1,1) == "k" ~ "tumor_specimen_available",
                       substring(var_patt,1,1) == "l" ~ "gene_test_expression_analysis")

    if(is.na(expr_str))
      ps_tmp <- subset(redcap_data, redcap_repeat_instrument == event)
    else
      ps_tmp <- subset(redcap_data, redcap_repeat_instrument == event & eval(parse(text = expr_str)))
  }

  if(nrow(ps_tmp) != 0){
    check_summary <- getMissUnkCheckbox(ps_tmp, var_patt, check_summary)

  }
  
  gc()
}

check_summary <- check_summary[gtools::mixedorder(check_summary$var),] %>%
  mutate(perc_miss = formattable::percent(perc_miss)) %>%
  mutate(perc_miss_unk = formattable::percent(perc_miss_unk)) %>%
  mutate(form = case_when(form == "preliminary" ~ "Preliminary",
                          form == "demographic_life_style" ~ "Demographic & life style",
                          form == "previous_cancer_gensyndrome" ~ "Previous cancer gen.syndrome",
                          form == "cancer_under_study" ~ "Cancer under study",
                          form == "staging_procedures_and_stage" ~ "Staging procedures and stage",
                          form == "cancer_under_study_treatment" ~ "Cancer Under Study Treatment",
                          form %in% grep("progressionrecurrencepersistent_disease", unique(metadata$form_name), value = TRUE) ~ "Progression/Recurrence/Persistent disease",
                          form == "status_of_patient_at_fup" ~ "Status of patient at fup",
                          form == "adverse_events" ~ "Adverse events",
                          form == "imaging_available" ~ "Imaging available",
                          form == "tumor_specimen_available" ~ "Tumor specimen available",
                          form == "gene_test_expression_analysis" ~ "Gene test expression analysis")) %>%
  select(-c("var")) %>%
  mutate(var_lab = gsub("<.*?>", "", var_lab)) %>%
  filter(var_lab != "Instrument status:")

colnames(check_summary) <- c("Variable", "Form", "Denominator","% missing", "% missing + unknown")

# COMPLETED PATIENTS ------
data_non_rep <- filter(redcap_data, redcap_event_name == "non_repeatable_arm_1")
# Instrument 1-6, 8 completed, all the 7s missing ----
pts_complete_1 <- data_non_rep %>% filter(if_all(c("a07_complete", "b24_complete", "c28_complete", "d34_complete", "e57_complete", "f107_complete", "h06_complete"), ~ . == 1) &
                                            if_all(c("g1_119_complete", "g2_119_complete", "g3_119_complete", "g4_119_complete", "g5_119_complete", "g6_119_complete", "g7_119_complete", "g8_119_complete", "g9_119_complete", "g10_119_complete"), ~ is.na(.)))

case_complete <- data.frame("status" = "Complete",
                            "description" = "Instrument 1-6, 8 completed, all the 7s missing",
                            "n_patients" = nrow(pts_complete_1),
                            "n_error" = nrow(filter(pts_complete_1, a01_id %in% check_failed$a01_id)),
                            "perc" = nrow(filter(pts_complete_1, a01_id %in% check_failed$a01_id))/nrow(pts_complete_1))

# Instrument 1-6, 8 completed and for all the Instruments 7  the first one completed and the others missing
pts_complete_2 <- data_non_rep %>% filter(if_all(c("a07_complete", "b24_complete", "c28_complete", "d34_complete", "e57_complete", "f107_complete", "h06_complete", "g1_119_complete"), ~ . == 1) &
                                            if_all(c("g2_119_complete", "g3_119_complete", "g4_119_complete", "g5_119_complete", "g6_119_complete", "g7_119_complete", "g8_119_complete", "g9_119_complete", "g10_119_complete"), ~ is.na(.)))
case_complete <- rbind.fill(case_complete, data.frame("status" = "Complete",
                                                      "description" = "Instrument 1-6, 8 completed and for all the Instruments 7 the first one completed and the others missing",
                                                      "n_patients" = nrow(pts_complete_2),
                                                      "n_error" = nrow(filter(pts_complete_2, a01_id %in% check_failed$a01_id)),
                                                      "perc" = nrow(filter(pts_complete_2, a01_id %in% check_failed$a01_id))/nrow(pts_complete_2)))

# Instrument 1-6, 8 completed and for all the Instruments 7  the first two completed and the others missing
pts_complete_3 <- data_non_rep %>% filter(if_all(c("a07_complete", "b24_complete", "c28_complete", "d34_complete", "e57_complete", "f107_complete", "h06_complete", "g1_119_complete", "g2_119_complete"), ~ . == 1) &
                                            if_all(c("g3_119_complete", "g4_119_complete", "g5_119_complete", "g6_119_complete", "g7_119_complete", "g8_119_complete", "g9_119_complete", "g10_119_complete"), ~ is.na(.)))

case_complete <- rbind.fill(case_complete, data.frame("status" = "Complete",
                                                      "description" = "Instrument 1-6, 8 completed and for all the Instruments 7 the first two completed and the others missing",
                                                      "n_patients" = nrow(pts_complete_3),
                                                      "n_error" = nrow(filter(pts_complete_3, a01_id %in% check_failed$a01_id)),
                                                      "perc" = nrow(filter(pts_complete_3, a01_id %in% check_failed$a01_id))/nrow(pts_complete_3)))

# Instrument 1-6, 8 completed and for all the Instruments 7  the first three completed and the others missing
pts_complete_4 <- data_non_rep %>% filter(if_all(c("a07_complete", "b24_complete", "c28_complete", "d34_complete", "e57_complete", "f107_complete", "h06_complete", "g1_119_complete", "g2_119_complete", "g3_119_complete"), ~ . == 1) &
                                            if_all(c("g4_119_complete", "g5_119_complete", "g6_119_complete", "g7_119_complete", "g8_119_complete", "g9_119_complete", "g10_119_complete"), ~ is.na(.)))

case_complete <- rbind.fill(case_complete, data.frame("status" = "Complete",
                                                      "description" = "Instrument 1-6, 8 completed and for all the Instruments 7 the first three completed and the others missing",
                                                      "n_patients" = nrow(pts_complete_4),
                                                      "n_error" = nrow(filter(pts_complete_4, a01_id %in% check_failed$a01_id)),
                                                      "perc" = nrow(filter(pts_complete_4, a01_id %in% check_failed$a01_id))/nrow(pts_complete_4)))

# Instrument 1-6, 8 completed and for all the Instruments 7  the first four completed and the others missing
pts_complete_5 <- data_non_rep %>% filter(if_all(c("a07_complete", "b24_complete", "c28_complete", "d34_complete", "e57_complete", "f107_complete", "h06_complete", "g1_119_complete", "g2_119_complete", "g3_119_complete", "g4_119_complete"), ~ . == 1) &
                                            if_all(c("g5_119_complete", "g6_119_complete", "g7_119_complete", "g8_119_complete", "g9_119_complete", "g10_119_complete"), ~ is.na(.)))

case_complete <- rbind.fill(case_complete, data.frame("status" = "Complete",
                                                      "description" = "Instrument 1-6, 8 completed and for all the Instruments 7 the first four completed and the others missing",
                                                      "n_patients" = nrow(pts_complete_5),
                                                      "n_error" = nrow(filter(pts_complete_5, a01_id %in% check_failed$a01_id)),
                                                      "perc" = nrow(filter(pts_complete_5, a01_id %in% check_failed$a01_id))/nrow(pts_complete_5)))

# Instrument 1-6, 8 completed and for all the Instruments 7  the first five completed and the others missing
pts_complete_6 <- data_non_rep %>% filter(if_all(c("a07_complete", "b24_complete", "c28_complete", "d34_complete", "e57_complete", "f107_complete", "h06_complete", "g1_119_complete", "g2_119_complete", "g3_119_complete", "g4_119_complete", "g5_119_complete"), ~ . == 1) &
                                            if_all(c("g6_119_complete", "g7_119_complete", "g8_119_complete", "g9_119_complete", "g10_119_complete"), ~ is.na(.)))

case_complete <- rbind.fill(case_complete, data.frame("status" = "Complete",
                                                      "description" = "Instrument 1-6, 8 completed and for all the Instruments 7 the first five completed and the others missing",
                                                      "n_patients" = nrow(pts_complete_6),
                                                      "n_error" = nrow(filter(pts_complete_6, a01_id %in% check_failed$a01_id)),
                                                      "perc" = nrow(filter(pts_complete_6, a01_id %in% check_failed$a01_id))/nrow(pts_complete_6)))

# Instrument 1-6, 8 completed and for all the Instruments 7  the first six completed and the others missing
pts_complete_7 <- data_non_rep %>% filter(if_all(c("a07_complete", "b24_complete", "c28_complete", "d34_complete", "e57_complete", "f107_complete", "h06_complete", "g1_119_complete", "g2_119_complete", "g3_119_complete", "g4_119_complete", "g5_119_complete", "g6_119_complete"), ~ . == 1) &
                                            if_all(c("g7_119_complete", "g8_119_complete", "g9_119_complete", "g10_119_complete"), ~ is.na(.)))

case_complete <- rbind.fill(case_complete, data.frame("status" = "Complete",
                                                      "description" = "Instrument 1-6, 8 completed and for all the Instruments 7 the first six completed and the others missing",
                                                      "n_patients" = nrow(pts_complete_7),
                                                      "n_error" = nrow(filter(pts_complete_7, a01_id %in% check_failed$a01_id)),
                                                      "perc" = nrow(filter(pts_complete_7, a01_id %in% check_failed$a01_id))/nrow(pts_complete_7)))

# Instrument 1-6, 8 completed and for all the Instruments 7  the first seven completed and the others missing
pts_complete_8 <- data_non_rep %>% filter(if_all(c("a07_complete", "b24_complete", "c28_complete", "d34_complete", "e57_complete", "f107_complete", "h06_complete", "g1_119_complete", "g2_119_complete", "g3_119_complete", "g4_119_complete", "g5_119_complete", "g6_119_complete", "g7_119_complete"), ~ . == 1) &
                                            if_all(c("g8_119_complete", "g9_119_complete", "g10_119_complete"), ~ is.na(.)))

case_complete <- rbind.fill(case_complete, data.frame("status" = "Complete",
                                                      "description" = "Instrument 1-6, 8 completed and for all the Instruments 7 the first seven completed and the others missing",
                                                      "n_patients" = nrow(pts_complete_8),
                                                      "n_error" = nrow(filter(pts_complete_8, a01_id %in% check_failed$a01_id)),
                                                      "perc" = nrow(filter(pts_complete_8, a01_id %in% check_failed$a01_id))/nrow(pts_complete_8)))

# Instrument 1-6, 8 completed and for all the Instruments 7  the first eight completed and the others missing
pts_complete_9 <- data_non_rep %>% filter(if_all(c("a07_complete", "b24_complete", "c28_complete", "d34_complete", "e57_complete", "f107_complete", "h06_complete", "g1_119_complete", "g2_119_complete", "g3_119_complete", "g4_119_complete", "g5_119_complete", "g6_119_complete", "g7_119_complete", "g8_119_complete"), ~ . == 1) &
                                            if_all(c("g9_119_complete", "g10_119_complete"), ~ is.na(.)))

case_complete <- rbind.fill(case_complete, data.frame("status" = "Complete",
                                                      "description" = "Instrument 1-6, 8 completed and for all the Instruments 7 the first eight completed and the others missing",
                                                      "n_patients" = nrow(pts_complete_9),
                                                      "n_error" = nrow(filter(pts_complete_9, a01_id %in% check_failed$a01_id)),
                                                      "perc" = nrow(filter(pts_complete_9, a01_id %in% check_failed$a01_id))/nrow(pts_complete_9)))

# Instrument 1-6, 8 completed and for all the Instruments 7  the first nine completed and the others missing
pts_complete_10 <- data_non_rep %>% filter(if_all(c("a07_complete", "b24_complete", "c28_complete", "d34_complete", "e57_complete", "f107_complete", "h06_complete", "g1_119_complete", "g2_119_complete", "g3_119_complete", "g4_119_complete", "g5_119_complete", "g6_119_complete", "g7_119_complete", "g8_119_complete", "g9_119_complete"), ~ . == 1) &
                                             if_all(c("g10_119_complete"), ~ is.na(.)))

case_complete <- rbind.fill(case_complete, data.frame("status" = "Complete",
                                                      "description" = "Instrument 1-6, 8 completed and for all the Instruments 7 the first nine completed and the others missing",
                                                      "n_patients" = nrow(pts_complete_10),
                                                      "n_error" = nrow(filter(pts_complete_10, a01_id %in% check_failed$a01_id)),
                                                      "perc" = nrow(filter(pts_complete_10, a01_id %in% check_failed$a01_id))/nrow(pts_complete_10)))

# Instrument 1-6, 8 completed and all the Instruments 7  are completed
pts_complete_11 <- data_non_rep %>% filter(if_all(c("a07_complete", "b24_complete", "c28_complete", "d34_complete", "e57_complete", "f107_complete", "h06_complete", "g1_119_complete", "g2_119_complete", "g3_119_complete", "g4_119_complete", "g5_119_complete", "g6_119_complete", "g7_119_complete", "g8_119_complete", "g9_119_complete", "g10_119_complete"), ~ . == 1))

case_complete <- rbind.fill(case_complete, data.frame("status" = "Complete",
                                                      "description" = "Instrument 1-6, 8 completed and all the Instruments 7  are completed",
                                                      "n_patients" = nrow(pts_complete_11),
                                                      "n_error" = nrow(filter(pts_complete_11, a01_id %in% check_failed$a01_id)),
                                                      "perc" = nrow(filter(pts_complete_11, a01_id %in% check_failed$a01_id))/nrow(pts_complete_11)))

# Instrument 1-6, 8 completed and at least one instrument 7 incompleted
pts_complete_12 <- data_non_rep %>% filter(if_all(c("a07_complete", "b24_complete", "c28_complete", "d34_complete", "e57_complete", "f107_complete", "h06_complete"), ~ . == 1) &
                                             if_any(c("g1_119_complete", "g2_119_complete", "g3_119_complete", "g4_119_complete", "g5_119_complete", "g6_119_complete", "g7_119_complete", "g8_119_complete", "g9_119_complete", "g10_119_complete"), ~ . == 2))

case_complete <- rbind.fill(case_complete, data.frame("status" = "Incomplete",
                                                      "description" = "Instrument 1-6, 8 completed and at least one instrument 7 incompleted",
                                                      "n_patients" = nrow(pts_complete_12),
                                                      "n_error" = nrow(filter(pts_complete_12, a01_id %in% check_failed$a01_id)),
                                                      "perc" = nrow(filter(pts_complete_12, a01_id %in% check_failed$a01_id))/nrow(pts_complete_12)))

# At least one Instrument 1-6 incompleted and the others completed, 8 completed
pts_complete_13 <- data_non_rep %>% filter(if_all(c("a07_complete", "b24_complete", "c28_complete", "d34_complete", "e57_complete", "f107_complete", "h06_complete"), ~ !is.na(.)) &
                                             if_any(c("a07_complete", "b24_complete", "c28_complete", "d34_complete", "e57_complete", "f107_complete", "h06_complete"), ~ . == 2))

case_complete <- rbind.fill(case_complete, data.frame("status" = "Incomplete",
                                                      "description" = "At least one Instrument 1-6 or 8 incompleted (without missing)",
                                                      "n_patients" = nrow(pts_complete_13),
                                                      "n_error" = nrow(filter(pts_complete_13, a01_id %in% check_failed$a01_id)),
                                                      "perc" = nrow(filter(pts_complete_13, a01_id %in% check_failed$a01_id))/nrow(pts_complete_13)))

pts_complete_14 <- data_non_rep %>% filter(if_any(c("a07_complete", "b24_complete", "c28_complete", "d34_complete", "e57_complete", "f107_complete", "h06_complete"), ~ is.na(.)))

case_complete <- rbind.fill(case_complete, data.frame("status" = "Missing",
                                                      "description" = "At least one Instrument 1-6 or 8 missing",
                                                      "n_patients" = nrow(pts_complete_14),
                                                      "n_error" = nrow(filter(pts_complete_14, a01_id %in% check_failed$a01_id)),
                                                      "perc" = nrow(filter(pts_complete_14, a01_id %in% check_failed$a01_id))/nrow(pts_complete_14)))


case_complete <- case_complete %>% mutate(n_error = ifelse(n_patients == 0, 0, n_error)) %>%
  mutate(perc = ifelse(n_patients == 0, 0, perc)) %>%
  mutate(perc = formattable::percent(perc))

colnames(case_complete) <- c("Status", "Description","N pts", "N pts with at least one error", "% pts with at least one error over total n pts")

# Staging & Treatments ---------
###### N.B. per trattamento considerare sempre e solo clinical stage
data_trt <- redcap_data %>% subset(redcap_event_name == "non_repeatable_arm_1") %>%
  mutate(treatment = case_when(
    # SOLO UN TRATTAMENTO
    f01_surgery %in% c(1,2) & !(f53_radio %in% c(1,2)) & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE), inherits = TRUE) %in% c(1,2)) & f02_datesurg <= (d01_diagdate + 60) ~ "Only surgery",
    f53_radio %in% c(1,2) & !(f01_surgery %in% c(1,2)) & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE), inherits = TRUE) %in% c(1,2)) & f80_radiostartdate <= (d01_diagdate + 60) ~ "Only radiotherapy",
    f30_1_combofsystrt == 1 & !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) & f33_1_startdate_syst <= (d01_diagdate + 60) ~ "Only chemo",
    f30_1_combofsystrt == 2 & !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) & f33_1_startdate_syst <= (d01_diagdate + 60) ~ "Only immuno",
    f30_1_combofsystrt == 3 & !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) & f33_1_startdate_syst <= (d01_diagdate + 60) ~ "Only target",
    # SOLO CONCOMITANTI
    !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1 & f32_1_systsetting == 2 & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) & min(c(f33_1_startdate_syst, f80_radiostartdate)) <= (d01_diagdate + 60) &
      max(c(f33_1_startdate_syst, f80_radiostartdate)) <= min(c(f33_1_startdate_syst, f80_radiostartdate)) + 7 & max(c(f34_1_enddate_syst, f81_radioendate)) <= min(c(f34_1_enddate_syst, f81_radioendate)) + 7 ~ "Concomitant chemo/radio",
    # CHIRURGIA + altro
    f01_surgery %in% c(1,2) & f55_radiosett == 3 & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE), inherits = TRUE) %in% c(1,2)) & f02_datesurg <= (d01_diagdate + 60) & f80_radiostartdate <= (f02_datesurg + 75) ~ "Surgery + postoperative radiotherapy",
    f01_surgery %in% c(1,2) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 1 & f32_1_systsetting == 3 & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) & f02_datesurg <= (d01_diagdate + 60) & f33_1_startdate_syst <= (f02_datesurg + 60) ~ "Surgery + adjuvant chemo",
    f01_surgery %in% c(1,2) & f55_radiosett == 4 & f30_1_combofsystrt == 1 & f32_1_systsetting == 2 & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) & f02_datesurg <= (d01_diagdate + 60) &
      min(c(f33_1_startdate_syst, f80_radiostartdate)) <= (f02_datesurg + 75) & max(c(f33_1_startdate_syst, f80_radiostartdate)) <= min(c(f33_1_startdate_syst, f80_radiostartdate)) + 7 & max(c(f34_1_enddate_syst, f81_radioendate)) <= min(c(f34_1_enddate_syst, f81_radioendate)) + 7 ~ "Surgery + Postoperative radio concomitant to chemotherapy",
    # RADIO + altro
    !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1 & f32_1_systsetting == 3 & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) & f80_radiostartdate <= (d01_diagdate + 60) & f33_1_startdate_syst <= (f81_radioendate + 45) ~ "Radio + adjuvant chemo",
    !(f01_surgery %in% c(1,2)) & f55_radiosett == 6 & f30_1_combofsystrt == 1 & f32_1_systsetting == 2 & f30_2_combofsystrt == 1 & f32_2_systsetting == 3 & !(f29_3_systrt %in% c(1,2)) & min(c(f33_1_startdate_syst, f80_radiostartdate)) <= (d01_diagdate + 60) &
      max(c(f33_1_startdate_syst, f80_radiostartdate)) <= min(c(f33_1_startdate_syst, f80_radiostartdate)) + 7 & max(c(f34_1_enddate_syst, f81_radioendate)) <= min(c(f34_1_enddate_syst, f81_radioendate)) + 7 &
      f33_2_startdate_syst <= max(c(f33_1_startdate_syst, f80_radiostartdate)) + 45 ~ "Concomitant chemo/radio + adjuvant chemo",
    # CHEMO + altro
    !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 1 & f30_2_combofsystrt == 2 & !(f29_3_systrt %in% c(1,2)) &
      all(c(f33_1_startdate_syst, f33_2_startdate_syst) <= (d01_diagdate + 60)) ~ "Chemo + immuno",
    !(f01_surgery %in% c(1,2)) & f53_radio %in% c(1,2) & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 &
      !any(mget(grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) &
      f33_1_startdate_syst <= (d01_diagdate + 60) & f80_radiostartdate <= (f34_1_enddate_syst + 30) ~ "Neo-adjuvant chemo + radio",
    f01_surgery %in% c(1,2) & !(f53_radio %in% c(1,2)) & f30_1_combofsystrt == 1  & f32_1_systsetting == 1 &
      !any(mget(grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE)[-1], inherits = TRUE) %in% c(1,2)) &
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
    !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE), inherits = TRUE) %in% c(1,2)) & !(f99_othertrt %in% c(1,2)) & f105_clinicltrial == 1 ~ "Clinical trial only",
    # NON TRATTATO
    !(f01_surgery %in% c(1,2)) & !(f53_radio %in% c(1,2)) & !any(mget(grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE), inherits = TRUE) %in% c(1,2)) & !(f99_othertrt %in% c(1,2)) ~ "No treatment",
    # OTHER
    TRUE ~ "Other")) %>%
  select(c("a01_id", "d11_siterare", "d11_sitecomrar","e34_cstage", "treatment")) %>%
  mutate(e34_cstage = case_when(e34_cstage == 999 ~ "Unknown",
                                e34_cstage == 1 ~ "0",
                                e34_cstage == 2 ~ "I",
                                e34_cstage == 3 ~ "II",
                                e34_cstage == 4 ~ "III",
                                e34_cstage == 5 ~ "IV",
                                e34_cstage == 6 ~ "IVA",
                                e34_cstage == 7 ~ "IVB",
                                e34_cstage == 8 ~ "IVC")) %>%
  mutate(site = case_when(d11_siterare == 1 | d11_sitecomrar == 1 ~ "Nasal cavity and paranasal sinuses",
                          d11_siterare == 2 | d11_sitecomrar == 2 ~ "Nasopharynx",
                          d11_siterare %in% c(3,4,5) | d11_sitecomrar %in% c(3,4,5) ~ "Parotid gland; Submandibular gland; Sublingual gland",
                          d11_siterare == 6 | d11_sitecomrar == 6 ~ "Middle ear",
                          # !is.na(d11_siterare) | !is.na(d11_sitecomrar) ~ "Other",
                          TRUE ~ "Other")) %>%
  plyr::count(c("e34_cstage", "treatment", "site")) %>%
  mutate(e34_cstage = ifelse(is.na(e34_cstage), "Stage not defined", e34_cstage))

lev_stage <- factor(unique(data_trt$e34_cstage),
                    levels = c("0", "I", "II", "III", "IV", "IVA", "IVB", "IVC", "Unknown", "Stage not defined"))
lev_trt <- factor(unique(data_trt$treatment),
                  levels = c("Only surgery", "Only radiotherapy", "Only chemo", "Only immuno", "Only target", "Concomitant chemo/radio", "Surgery + postoperative radiotherapy", "Surgery + adjuvant chemo", "Surgery + Postoperative radio concomitant to chemotherapy", "Radio + adjuvant chemo", "Concomitant chemo/radio + adjuvant chemo", "Chemo + immuno",
                             "Neo-adjuvant chemo + radio", "Neo-adjuvant chemo + surgery", "Neo-adjuvant chemo + concomitant radio/chemo", "Neo-adjuvant chemo + concomitant radio/chemo + chemo adjuvant", "Neo-adjuvant chemo + radio + chemo adjuvant", "Immuno + target", "Immuno + chemo", "Clinical trial only", "No treatment", "Other"))
lev_site <- factor(unique(data_trt$site),
                   levels = c("Nasal cavity and paranasal sinuses", "Nasopharynx", "Parotid gland; Submandibular gland; Sublingual gland", "Middle ear", "Other"))

stage_per_treat_tot <- setNames(vector("list", length(levels(lev_site))), levels(lev_site))

for (k in names(stage_per_treat_tot)) {
  data_trt_tmp <- data_trt %>%
    filter(site == k)

  stage_per_treat <- setNames(data.frame(matrix(ncol = length(levels(lev_trt)), nrow = length(levels(lev_stage))), row.names = levels(lev_stage)), levels(lev_trt))

  for (i in 1:nrow(stage_per_treat)) {
    stage <- rownames(stage_per_treat)[i]
    for (j in 1:ncol(stage_per_treat)) {
      trt <- colnames(stage_per_treat)[j]
      stage_per_treat[i,j] <- ifelse(length(data_trt_tmp[which(data_trt_tmp$e34_cstage == stage & data_trt_tmp$treatment == trt), 'freq']),
                                     data_trt_tmp[which(data_trt_tmp$e34_cstage == stage & data_trt_tmp$treatment == trt), 'freq'], NA)
    }
  }

  stage_per_treat <- stage_per_treat %>%
    mutate_all(~replace(., is.na(.), 0)) %>%
    mutate(`No treatment` = `No treatment`  + `Clinical trial only`)

  stage_per_treat_tot[[k]] <- stage_per_treat
  
  gc()
}

# Core variables --------
core_var <- c("a05_date", "a06_phase", "b04_sex", "b06_resid", "b07_smoke", "b11_smokepy_auto", "b13_alc", "b14_carc", "b19_comorb",
              "c01_prevc", "c02_prevcsite", "d01_diagdate", "d03_diagage_au", "d05_histo", "d06_histosubg_sq", "d07_histosubg_ade", "d08_histosubg_net",
              "d09_histosubg_odo", "d11_siterare", "d11_sitecomrar", "d14_subsite_nasal", "d15_subsite_naso", "d16_subsite_hypo", "d17_subsite_oro",
              "d18_subsite_lar", "d19_subsite_oc", "d20_subsite_lip", "d21_ebv", "e01_stagehosp", "e02_im_primary", "e12_im_neck", "e21_im_m", "e32_c_ene",
              "e34_cstage", "e34_cstageed", "e39_p_ene", "e41_margins", "e43_sentnode", "e54_pstage", "e54_pstageed", "f01_surgery", "f02_datesurg", "f03_surgintent",
              "f07_reconstr", "f11_necksurg", "f12_datenecksurg", "f20_surgm", "f22_datesurgm", "f23_complic1", "f24_complic2", "f25_unplsurg", "f26_dateunplsurg",
              grep("f29_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE), grep("f30_[[:digit:]]_combofsystrt", colnames(redcap_data), value = TRUE), grep("f31_[[:digit:]]_systintent", colnames(redcap_data), value = TRUE),
              grep("f32_[[:digit:]]_systsetting", colnames(redcap_data), value = TRUE), colnames(redcap_data)[grepl("f33_[[:digit:]]_startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], colnames(redcap_data)[grepl("f34_[[:digit:]]_enddate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))],
              grep("f35_[[:digit:]]_numcycle", colnames(redcap_data), value = TRUE), grep("f36_[[:digit:]]_regimen", colnames(redcap_data), value = TRUE), grep("f44_[[:digit:]]_syst_reasonend", colnames(redcap_data), value = TRUE),
              grep("f45_[[:digit:]]_regchange", colnames(redcap_data), value = TRUE), colnames(redcap_data)[grepl("f48_[[:digit:]]_reg2stdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], colnames(redcap_data)[grepl("f49_[[:digit:]]_reg2endate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))],
              "f53_radio", "f54_radiointent", "f55_radiosett", "f57_beamqual", "f63_totdose", "f64_fractionsize", "f65_numfraction", "f80_radiostartdate", "f81_radioendate", "f84_ott", "f84_ottman",
              "f85_radiosite", "f92_trtcomplete", grep("g[[:digit:]]+_01_progrel", colnames(redcap_data), value = TRUE), colnames(redcap_data)[grepl("g[[:digit:]]+_03_date", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], grep("g[[:digit:]]+_04_local", colnames(redcap_data), value = TRUE),
              grep("g[[:digit:]]+_05_regional", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_06_meta", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_08_surgery", colnames(redcap_data), value = TRUE), colnames(redcap_data)[grepl("g[[:digit:]]+_09_datesurg", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))],
              grep("g[[:digit:]]+_10_surgintent", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_13_margins", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_14_reconstr", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_15_necksurg", colnames(redcap_data), value = TRUE),
              colnames(redcap_data)[grepl("g[[:digit:]]+_16_datenecksurg", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], grep("g[[:digit:]]+_24_ene", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_30_surgm", colnames(redcap_data), value = TRUE), colnames(redcap_data)[grepl("g[[:digit:]]+_32_datesurgm", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))],
              grep("g[[:digit:]]+_33_1complic", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_34_2complic", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_35_[[:digit:]]_systrt", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_36_[[:digit:]]_combofsystrt", colnames(redcap_data), value = TRUE),
              grep("g[[:digit:]]+_37_[[:digit:]]_systintent", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_38_[[:digit:]]_systsetting", colnames(redcap_data), value = TRUE),
              colnames(redcap_data)[grepl("g[[:digit:]]+_39_[[:digit:]]_startdate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], colnames(redcap_data)[grepl("g[[:digit:]]+_40_[[:digit:]]_enddate_syst", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], grep("g[[:digit:]]+_41_[[:digit:]]_numcycle", colnames(redcap_data), value = TRUE),
              grep("g[[:digit:]]+_42_[[:digit:]]_regimen", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_52_[[:digit:]]_syst_reasonend", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_53_[[:digit:]]_regchange", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_63_radio", colnames(redcap_data), value = TRUE),
              grep("g[[:digit:]]+_64_reradiation", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_65_rerad_reason", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_66_radiointent", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_67_radiosett", colnames(redcap_data), value = TRUE),
              grep("g[[:digit:]]+_69_beamqual", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_75_totd", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_76_fractionsize", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_77_numfraction", colnames(redcap_data), value = TRUE),
              colnames(redcap_data)[grepl("g[[:digit:]]+_92_radiostartdate", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], colnames(redcap_data)[grepl("g[[:digit:]]+_93_radioendate", colnames(redcap_data)) & !grepl("unk", colnames(redcap_data))], grep("g[[:digit:]]+_96_ott", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_97_radiosite", metadata$field_name, value = TRUE),
              grep("g[[:digit:]]+_104_radio_complete", colnames(redcap_data), value = TRUE), grep("g[[:digit:]]+_119_wfw", colnames(redcap_data), value = TRUE), "h01_status", "h02_datelasfup")

metadata_core <- metadata %>% subset(field_name %in% core_var)

# remove patients with at least one missining or unknown core variable
core_pts <- redcap_data %>% subset(redcap_event_name == "non_repeatable_arm_1")

for (i in 1:nrow(metadata_core)) {
  var_tmp = metadata_core$field_name[i]
  form_complete = ifelse(substring(var_tmp,1,1) == "g",
                         grep(paste0(substring(var_tmp,1,2),"_[[:digit:]]+_complete"), metadata$field_name, value = TRUE),
                         grep(paste0(substring(var_tmp,1,1),"[[:digit:]]+_complete"), metadata$field_name, value = TRUE))
  expr_str = renderLogic(metadata_core$branching_logic[i])
  if(metadata_core$field_type[i] != "checkbox"){
    if(!is.na(expr_str)){
      pts_tmp <- core_pts %>% subset(get(form_complete) == 1 & eval(parse(text = expr_str))) %>%
        subset(is.na(get(var_tmp)) | get(var_tmp) %in% c(999, 9999))
    } else {
      pts_tmp <- core_pts %>% subset(get(form_complete) == 1) %>%
        subset(is.na(get(var_tmp)) | get(var_tmp) %in% c(999, 9999))
    }
  } else {
    cond <- ifelse(length(grep(paste0(var_tmp, "___999"), colnames(core_pts))),
                   'all(is.na(mget(grep(var_tmp, colnames(core_pts), value = TRUE), inherits = TRUE))) | get(grep(paste0(var_tmp, "___999"), colnames(core_pts), value = TRUE)) == 1',
                   'all(is.na(mget(grep(var_tmp, colnames(core_pts), value = TRUE), inherits = TRUE)))')
    if(!is.na(expr_str)){
      pts_tmp <- core_pts %>% subset(get(form_complete) == 1 & eval(parse(text = expr_str))) %>%
        subset(eval(parse(text = cond)))
    } else {
      pts_tmp <- core_pts %>% subset(get(form_complete) == 1) %>%
        subset(eval(parse(text = cond)))
    }
  }

  core_pts <- subset(core_pts, !(a01_id %in% pts_tmp$a01_id))
  
  gc()
}

# dataframe for summary report
core <- data.frame("patients" = length(unique(redcap_data$a01_id)), "pts_core" = nrow(core_pts),
                   "perc" = formattable::percent(nrow(core_pts)/length(unique(redcap_data$a01_id))))

colnames(core) <- c("N patients", "N patients with core variables", "% patients with core variables")

# Add a column 'core' to data:
# 1 = core complete
# 0 = core complete
redcap_data <- redcap_data %>% mutate(core = ifelse(a01_id %in% core_pts$a01_id, 1, 0))

# write summary report
file_name <- paste0(properties$id_centro,'-',id_analysis,'-QC-summary-', format(Sys.Date(), "%y%m%d"), '.xlsx')

# create workbook
wb <- createWorkbook()

# add worksheet
addWorksheet(wb, "Missing & Unknown")
addWorksheet(wb, "Completed patients")
addWorksheet(wb, "Staging & Treatments")
addWorksheet(wb, "Core variables")

# write data on worksheet
# Missing & Unknown
writeData(wb,sheet = "Missing & Unknown", check_summary, borders = "all", borderColour = "#999999",
          headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))
setColWidths(wb,sheet = "Missing & Unknown",cols = 1:ncol(check_summary), widths = "auto")

# Completed patients
mergeCells(wb, "Completed patients", cols = 1, rows = 2:12)
mergeCells(wb, "Completed patients", cols = 1, rows = 13:14)
writeData(wb,sheet = "Completed patients", case_complete, borders = "all", borderColour = "#999999",
          headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))
setColWidths(wb,sheet = "Completed patients",cols = 1:ncol(case_complete), widths = "auto")

# Staging & Treatments
curr_row <- 1
for(i in seq_along(stage_per_treat_tot)[-length(seq_along(stage_per_treat_tot))]) {
  writeData(wb, "Staging & Treatments", names(stage_per_treat_tot)[i], startCol = 1, startRow = curr_row)
  addStyle(wb, sheet = 'Staging & Treatments', createStyle(textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "#999999", fontSize = 12, fontColour = "#cc0000"), rows = curr_row, cols = 1)
  writeData(wb, "Staging & Treatments", stage_per_treat_tot[[i]], startCol = 1, startRow = curr_row + 1, rowNames = TRUE, borders = "all", borderColour = "#999999",
            headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))
  curr_row <- curr_row + nrow(stage_per_treat_tot[[i]]) + 4
}
setColWidths(wb,sheet = "Staging & Treatments",cols = 1:ncol(stage_per_treat_tot[[1]]), widths = "auto")

# Core variables
writeData(wb,sheet = "Core variables", core, borders = "all", borderColour = "#999999",
          headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))
setColWidths(wb,sheet = "Core variables",cols = 1:ncol(core), widths = "auto")

# save to xlsx
saveWorkbook(wb, file_name, overwrite = TRUE)

# Copy in /data
file.copy(from = paste0("/opt/redcap_dq/environment/scripts/", file_name), "/opt/redcap_dq/environment/data")

# Copy in /vantage6-starter_head_and_neck-user-vol/_data
file.copy(from = paste0("/opt/redcap_dq/environment/data/", file_name), "/var/lib/docker/volumes/vantage6-starter_head_and_neck-user-vol/_data")
file.rename(from = paste0("/var/lib/docker/volumes/vantage6-starter_head_and_neck-user-vol/_data/", file_name),
            to = paste0("/var/lib/docker/volumes/vantage6-starter_head_and_neck-user-vol/_data/", ID_alg,"-QC-summary.xlsx"))

############################################## PS- PASS ############################################## 
# Salvo la lista dei pazienti che hanno passato i check in un csv (elimino tutti i pazienti che hanno almeno un check FAIL)
ps_pass <- redcap_data %>%
  filter(!(a01_id %in% check_failed$a01_id)) %>% #patients without errors
  select(c("a01_id", "redcap_event_name", "redcap_repeat_instrument", "redcap_repeat_instance", "a07_complete", "b24_complete","c28_complete",
           "d34_complete", "e57_complete", "f107_complete", grep("g[[:digit:]]+_119_complete", colnames(redcap_data), value = TRUE), "h06_complete", "i07_complete", "core")) #save completeness status and core

file_name <- paste0(properties$id_centro,'-',id_analysis,'-pset-pass-', format(Sys.Date(), "%y%m%d"), '.csv')
write.csv(ps_pass, file_name, row.names = FALSE, na = "")

# Copy in /data
file.copy(from = paste0("/opt/redcap_dq/environment/scripts/", file_name), "/opt/redcap_dq/environment/data")