##### Data Quality HN Core Data Registry 2.0 #####

# check ID algorithm
ID_alg <- paste0("TEST", format(Sys.Date(), "%y%m%d"),"_1")

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

# extract data from REDCap
redcap_data <- redcap_read(
  redcap_uri = properties$uri,
  token = properties$token, 
  export_data_access_groups = TRUE)$data

#extract metadata from REDCap
metadata <- redcap_metadata_read(
  redcap_uri = properties$uri,
  token = properties$token)$data

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


### Rule #6 --------------------------------------------------------------------
# Year of diagnosis (c03_prevcyear) not specified (if c01_prevc = yes)

data_rule6 <- ddply(subset(redcap_data, c28_complete == 1 & c01_prevc == 1), c("a01_id"), summarize,
                    category = "ERROR",
                    description = "Year of diagnosis is not specified",
                    form1 = "Previous cancer gen.syndrome",
                    event1 = "NON REPEATABLE",
                    instance1 = redcap_repeat_instance,
                    var1 = metadata$field_label[which(metadata$field_name == "c03_prevcyear")],
                    value1 = c03_prevcyear,
                    status = ifelse(!(is.na(value1)),'PASS','FAIL'))

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

# Copy in /opt/redcap_dq/environment/data/
file.copy(from = paste0("/opt/redcap_dq/environment/scripts/", file_name), "/opt/redcap_dq/environment/data")

# Copy in /vantage6-starter_head_and_neck-user-vol/_data
system(paste0("echo \'datafile=\"/opt/redcap_dq/environment/data/", file_name,"\";
if [[ \"$( docker ps -q -f name=vantage6-starter_head_and_neck-user)\" && \"$( docker container inspect -f '{{.State.Status}}' vantage6-starter_head_and_neck-user )\" == \"running\" ]]; then
      docker cp $datafile vantage6-starter_head_and_neck-user:/mnt/data/", ID_alg, "-QC-individual.xlsx
fi
cp $datafile /data' | bash"))

# system(paste0("container_name=\"vantage6-starter_head_and_neck-user\";
# datafile=\"/opt/redcap_dq/environment/data/", file_name,"\";
# if [[ \"$( docker ps -q -f name=$container_name)\" && \"$( docker container inspect -f '{{.State.Status}}' $container_name )\" == \"running\" ]]; then
#       docker cp $datafile $container_name:/mnt/data
# fi;
# cp $datafile /data"))

############################################## QC REPORT SUMMARY ##############################################
# Missing and unknown ---------
# create a new dataframe to count % of missing and % of missing+unknown
check_summary <- data.frame("var" = character(0), "var_lab" = character(0), "form" = character(0), "den" = integer(0), "perc_miss" = double(0), "perc_miss_unk" = double(0))

## Select variables with field type != descriptive, calculated or checkbox
metadata_tmp <- subset(metadata,!(field_type %in% c("descriptive","calc", "checkbox"))) %>%
  # tolgo anche a01_id e tutte quelle del tipo *_dayunk
  filter(!(field_name %in% c("a01_id", grep("(date)([[:graph:]]*)(_unk)", metadata$field_name, value = TRUE)))) %>%
  slice(1:3)


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

# Sort in alphabetic order and change columns names ----
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

# write summary report
file_name <- paste0(properties$id_centro,'-',id_analysis,'-QC-summary-', format(Sys.Date(), "%y%m%d"), '.xlsx')

# create workbook
wb <- createWorkbook()

# add worksheet
addWorksheet(wb, "Missing & Unknown")

# write data on worksheet
# Missing & Unknown
writeData(wb,sheet = "Missing & Unknown", check_summary, borders = "all", borderColour = "#999999",
          headerStyle = createStyle(border = c("top", "bottom", "left", "right"), borderColour = "#999999", textDecoration = "bold"))
setColWidths(wb,sheet = "Missing & Unknown",cols = 1:ncol(check_summary), widths = "auto")

# save to xlsx
saveWorkbook(wb, file_name, overwrite = TRUE)

# Copy in /opt/redcap_dq/environment/data
file.copy(from = paste0("/opt/redcap_dq/environment/scripts/", file_name), "/opt/redcap_dq/environment/data")

# Copy in /vantage6-starter_head_and_neck-user-vol/_data
system(paste0("echo \'datafile=\"/opt/redcap_dq/environment/data/", file_name,"\";
if [[ \"$( docker ps -q -f name=vantage6-starter_head_and_neck-user)\" && \"$( docker container inspect -f '{{.State.Status}}' vantage6-starter_head_and_neck-user )\" == \"running\" ]]; then
      docker cp $datafile vantage6-starter_head_and_neck-user:/mnt/data/", ID_alg, "-QC-summary.xlsx
fi
cp $datafile /data' | bash"))



# system(paste0("container_name=\"vantage6-starter_head_and_neck-user\";
# datafile=\"/opt/redcap_dq/environment/data/", file_name,"\";
# if [[ \"$( docker ps -q -f name=$container_name)\" && \"$( docker container inspect -f '{{.State.Status}}' $container_name )\" == \"running\" ]]; then
#       docker cp $datafile $container_name:/mnt/data
# fi;
# cp $datafile /data"))