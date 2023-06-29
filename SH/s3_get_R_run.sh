#TODO decide if more than 1 file to be executed can be on the remote repo
LOCK_FILE="/tmp/VANTAGE_PROCESS_LOCK_FILE"
general_log="/opt/redcap_dq/environment/logs/log.txt"

if ! [ -f "$LOCK_FILE" ]; then
	URL="https://biomeris-int-vantage-test.s3-eu-west-1.amazonaws.com/exec_1.R"
	script_wd="/opt/redcap_dq/environment/scripts/"
	logs="/opt/redcap_dq/environment/logs/"

	touch $LOCK_FILE

	# these variable are safe to be set only if the process isn't locked
	# otherwise they affect the previous execution that is not finished
	today=$(date +"%y%m%d_%H%M%S")
	log_folder="$logs""$today" 


	if ! [ -d "$log_folder" ]; then
		mkdir "$log_folder";
	fi

	if wget -T 20 -q "$URL" -P "$script_wd" > /dev/null; then
	
		#log.txt
		echo -n  $(date +"%x-%X") >> $general_log
		echo " folder $log_folder created" >> $general_log
	

		#log.txt
		echo -n  $(date +"%x-%X") >> $general_log
		echo " file exec_1.R downloaded from $URL" >> $general_log
		
		#log.txt
		echo -n  $(date +"%x-%X") >> $general_log
		echo " file exec_1.R moved to $script_wd" >> $general_log 

		cd "$script_wd"
		touch Rlog.txt
		Rscript exec_1.R > "Rlog.txt" 2>&1
		
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
		#echo " impossible to download file exec_1.R" >> $general_log
		echo " NTD" >> $general_log
		
		rm -rf "$log_folder";
		#echo -n  $(date +"%x-%X") >> $general_log
		#echo " folder $log_folder deleted" >> $general_log
		
	fi

	# even if there are errors this part of the script will be executed so 
	# the lock file will be removed only in the execution in which it is 
	# created
	rm $LOCK_FILE
	
	unset log_folder
	unset today
	unset logs
	unset script_wd
	unset URL
else
		echo -n  $(date +"%x-%X") >> $general_log
		echo " SKIPPED - Waiting for previous execution to finish." >> $general_log
fi