#!/bin/bash

# Output script name
output_script="download_test_jobs.sh"

# Initialize the script
echo "#!/bin/bash" > $output_script
echo "" >> $output_script

# Read each line from the CSV and create a download command
tail -n +2 wxs_file.csv | while IFS=',' read -r pair wgsid type egaf_id
do
    job_name="$egaf_id"
    log_file="${job_name}.log"

    echo "echo 'Starting download for $job_name...'" >> $output_script

    # Append the download command to the script (sequentially with &&)
    echo "pyega3 -cf credentials.json -c 28 fetch $job_name > $log_file 2>&1 && echo 'Download completed for $job_name. Logs are written to $log_file.'" >> $output_script

    echo "" >> $output_script
done

# Make the script executable
chmod +x $output_script

echo "Script $output_script has been generated. Run it with './$output_script'"

