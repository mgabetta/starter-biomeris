#TODO decide if more than 1 file to be executed can be on the remote repo 
URL="https://biomeris-int-vantage-test.s3-eu-west-1.amazonaws.com/exec_1.R"
script_wd="/opt/redcap_dq/environment/scripts/"
logs="/opt/redcap_dq/environment/logs/"
general_log="/opt/redcap_dq/environment/logs/log.txt"

today=$(date +"%y%m%d_%H%M%S")

log_folder="$logs""$today" 

if ! [ -d "$log_folder" ]; then
	mkdir "$log_folder";

	#log.txt
	echo -n  $(date +"%x-%X") >> $general_log
	echo " folder $log_folder created" >> $general_log
fi

if wget -T 20 -q "$URL" -P "$script_wd" > /dev/null; then

	#log.txt
	echo -n  $(date +"%x-%X") >> $general_log
	echo " file exec_1.R downloaded from $URL" >> $general_log
	
	#log.txt
	echo -n  $(date +"%x-%X") >> $general_log
	echo " file exec_1.R moved to $script_wd" >> $general_log 

	cd "$script_wd"
	touch Rlog.txt
	sudo Rscript exec_1.R > "Rlog.txt" 2>&1
	
	#log.txt
	echo -n  $(date +"%x-%X") >> $general_log
	echo " script exec_1.R run with exit code $?" >> $general_log
	
	mv ./* "$log_folder"
	
	#log.txt
	echo -n  $(date +"%x-%X") >> $general_log
	echo " file exec_1.R and all generated resources moved to $log_folder" >> $general_log
	
else
	#log.txt
	echo -n  $(date +"%x-%X") >> $general_log
	echo " impossible to download file exec_1.R" >> $general_log
fi



unset log_folder
unset today
unset general_log
unset logs
unset script_wd
unset URL