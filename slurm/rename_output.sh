#!/bin/bash
# cd to the specific results directory and then submit this script as 
# sh ../../ChinaWorldBankAidLocationSelectionAfrica/slurm/rename_output.sh
# Loop through each Causal Salience Map in the directory
for file in CSM_KW3_AvePool1_Tag*; do
    # Extract the part after "Tag"
    suffix="${file#*Tag}"
    suffix="${suffix%.pdf}"


    # Extract the part before "Tag"
    prefix="${file%%Tag*}"
    prefix="${prefix%_}"


    # Rename the file, reversing order of the parts 
    new_filename="${suffix}_${prefix}.pdf"
    mv "$file" "$new_filename"
done


# Loop through each Propensity Histogram in the directory
for file in Hist_KW3_AvePool1_Tag*; do
    # Extract the part after "Tag"
    suffix="${file#*Tag}"
    suffix="${suffix%.pdf}"


    # Extract the part before "Tag"
    prefix="${file%%Tag*}"
    prefix="${prefix%_}"


    # Rename the file, reversing order of the parts
    new_filename="${suffix}_${prefix}.pdf"
    mv "$file" "$new_filename"
done

